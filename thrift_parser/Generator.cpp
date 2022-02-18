/**
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Sergey Skoblikov, 2015-2022 Dmitry Ivanov
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "Generator.h"

#include <QDir>
#include <QFile>
#include <QMap>
#include <QSet>
#include <QStack>
#include <QString>
#include <QTextStream>

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>

namespace qevercloud_generator {

namespace {

////////////////////////////////////////////////////////////////////////////////

constexpr const char * auxiliaryMethodsDisclaimer =
    "    /**\n"
    "     * Methods below correspond to fields which are NOT set by QEverCloud "
    "itself.\n"
    "     * They exist for convenience of client code and are intended to be "
    "called\n"
    "     * and used by QEverCloud's client code if/when appropriate\n"
    "     */\n";

constexpr const char * blockSeparator =
    "////////////////////////////////////////"
    "////////////////////////////////////////";

constexpr const char * disclaimer =
    "/**\n"
    " * Original work: Copyright (c) 2014 Sergey Skoblikov\n"
    " * Modified work: Copyright (c) 2015-2022 Dmitry Ivanov\n"
    " *\n"
    " * This file is a part of QEverCloud project and is distributed under "
    "the terms\n"
    " * of MIT license:\n"
    " * https://opensource.org/licenses/MIT\n"
    " *\n"
    " * This file was generated from Evernote Thrift API\n"
    " */\n";

constexpr const char * indent = "    ";

constexpr const char * ln = "\n";

////////////////////////////////////////////////////////////////////////////////

[[nodiscard]] QString generatedFileOutputPath(
    const QString & outPath, const QString & section, const OutputFileType type)
{
    QString path = outPath;
    if (type == OutputFileType::Interface) {
        path += QStringLiteral("/include/qevercloud");
    }
    else if (type == OutputFileType::Implementation) {
        path += QStringLiteral("/src");
    }
    else if (type == OutputFileType::Test) {
        path += QStringLiteral("/tests");
    }
    else {
        throw std::logic_error(
            QString::fromUtf8("Wrong value of output file type flag: %1")
            .arg(static_cast<qint64>(type)).toStdString());
    }

    if (!section.isEmpty()) {
        path += QStringLiteral("/") + section;
    }

    return path;
}

[[nodiscard]] QString capitalize(const QString & input)
{
    if (input.isEmpty()) {
        return input;
    }

    if (input[0].isUpper()) {
        return input;
    }

    QString result;
    result.reserve(input.size());

    result.push_back(input.at(0).toUpper());
    for(int i = 1, size = input.size(); i < size; ++i) {
        result.push_back(input.at(i));
    }

    return result;
}

[[nodiscard]] QString decapitalize(const QString & input)
{
    if (input.isEmpty()) {
        return input;
    }

    if (input[0].isLower()) {
        return input;
    }

    QString result;
    result.reserve(input.size());

    result.push_back(input.at(0).toLower());
    for(int i = 1, size = input.size(); i < size; ++i) {
        result.push_back(input.at(i));
    }

    return result;
}

[[nodiscard]] QString fieldPropertyName(const Parser::Field & field)
{
    if (!field.m_overridePropertyName.isEmpty()) {
        return field.m_overridePropertyName;
    }

    QString res;
    QTextStream strm{&res};

    if (field.m_name.startsWith(QStringLiteral("is"))) {
        strm << decapitalize(field.m_name.mid(2));
    }
    else {
        strm << decapitalize(field.m_name);
    }

    return res;
}

[[nodiscard]] QString fieldSetterName(const Parser::Field & field)
{
    QString res;
    QTextStream strm{&res};
    strm << "set";

    if (field.m_name.startsWith(QStringLiteral("is"))) {
        strm << capitalize(field.m_name.mid(2));
    }
    else {
        strm << capitalize(field.m_name);
    }

    return res;
}

void ensureDirExists(const QString & path)
{
    QDir dir(path);
    if (!dir.exists())
    {
        bool res = dir.mkpath(dir.absolutePath());
        if (Q_UNLIKELY(!res)) {
            throw std::runtime_error(QString::fromUtf8(
                "Can't create directory for output files: %1")
                .arg(dir.absolutePath()).toStdString());
        }
    }
}

[[nodiscard]] Parser::Structures sortedStructsAndExceptions(
    const Parser & parser)
{
    Parser::Structures structsAndExceptions;
    structsAndExceptions << parser.structures();
    structsAndExceptions << parser.exceptions();

    std::sort(
        structsAndExceptions.begin(),
        structsAndExceptions.end(),
        [](const Parser::Structure & lhs, const Parser::Structure & rhs)
        {
            return lhs.m_name.localeAwareCompare(rhs.m_name) < 0;
        });

    return structsAndExceptions;
}

} // namespace

////////////////////////////////////////////////////////////////////////////////

Generator::OutputFileContext::OutputFileContext(
    const QString & fileName,
    const QString & outPath,
    const OutputFileType type,
    const QString & section)
{
    QString path = generatedFileOutputPath(outPath, section, type);
    ensureDirExists(path);

    m_file.setFileName(path + QStringLiteral("/") + fileName);
    if (!m_file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open outout file for writing: %1")
            .arg(m_file.fileName()).toStdString());
    }

    m_type = type;

    m_out.setDevice(&m_file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    m_out.setEncoding(QStringEncoder::Utf8);
#else
    m_out.setCodec("UTF-8");
#endif
}

////////////////////////////////////////////////////////////////////////////////

QString Generator::clearInclude(const QString & s) const
{
    for(const auto & inc: qAsConst(m_includeList)) {
        if (s.startsWith(inc)) {
            return s.mid(inc.length());
        }
    }

    return s;
}

std::optional<Parser::PrimitiveType::Type> Generator::aliasedPrimitiveType(
    const QString & primitiveTypeAlias) const
{
    const auto it = m_primitiveTypeAliases.find(primitiveTypeAlias);
    if (it != m_primitiveTypeAliases.end()) {
        return std::make_optional(it.value());
    }

    return std::nullopt;
}

QString Generator::aliasedTypeName(const QString & s) const
{
    if (const auto it = m_primitiveTypeAliases.find(s);
        it != m_primitiveTypeAliases.end())
    {
        return Parser::PrimitiveType::nativeTypeName(it.value());
    }

    if (m_stringTypeAliases.contains(s)) {
        return QStringLiteral("QString");
    }

    if (m_byteArrayTypeAliases.contains(s)) {
        return QStringLiteral("QByteArray");
    }

    return s;
}

QStringList Generator::additionalIncludesForFields(
    const Parser::Structure & s) const
{
    QStringList result;

    bool hasOptionalField = false;
    bool hasListField = false;
    bool hasSetField = false;
    bool hasMapField = false;

    for (const auto & f: s.m_fields)
    {
        if (f.m_required == Parser::Field::RequiredFlag::Optional) {
            hasOptionalField = true;
        }

        if (dynamic_cast<Parser::ListType*>(f.m_type.get())) {
            hasListField = true;
            continue;
        }

        if (dynamic_cast<Parser::SetType*>(f.m_type.get())) {
            hasSetField = true;
            continue;
        }

        if (dynamic_cast<Parser::MapType*>(f.m_type.get())) {
            hasMapField = true;
            continue;
        }
    }

    if (hasOptionalField) {
        result << QStringLiteral("<optional>");
    }

    if (hasListField) {
        result << QStringLiteral("<QList>");
    }

    if (hasSetField) {
        result << QStringLiteral("QSet");
    }

    if (hasMapField) {
        result << QStringLiteral("QMap");
    }

    return result;
}

QStringList Generator::dependentTypeNames(const Parser::Structure & s) const
{
    QSet<QString> dependentTypeNames;
    const auto processType =
        [this, &dependentTypeNames]
        (const std::shared_ptr<Parser::Type> & type) -> bool
        {
            const auto typeName = aliasedTypeName(
                typeToStr(type, {}, MethodType::TypeName));

            if (m_allStructs.contains(typeName) ||
                m_allExceptions.contains(typeName))
            {
                dependentTypeNames.insert(typeName);
                return true;
            }

            return false;
        };

    QStack<std::shared_ptr<Parser::Type>> typesStack;
    for (const auto & f: s.m_fields) {
        typesStack.push(f.m_type);
    }

    while (!typesStack.isEmpty())
    {
        auto type = typesStack.pop();
        if (processType(type)) {
            continue;
        }

        if (const auto * l = dynamic_cast<Parser::ListType*>(type.get())) {
            typesStack.push(l->m_valueType);
            continue;
        }

        if (const auto * s = dynamic_cast<Parser::SetType*>(type.get())) {
            typesStack.push(s->m_valueType);
            continue;
        }

        if (const auto * m = dynamic_cast<Parser::MapType*>(type.get())) {
            typesStack.push(m->m_keyType);
            typesStack.push(m->m_valueType);
            continue;
        }

        if (dynamic_cast<Parser::ByteArrayType*>(type.get())) {
            dependentTypeNames.insert(QStringLiteral("QByteArray"));
            continue;
        }

        if (dynamic_cast<Parser::StringType*>(type.get())) {
            dependentTypeNames.insert(QStringLiteral("QString"));
            continue;
        }
    }

#if QT_VERSION >= QT_VERSION_CHECK(5, 14, 0)
    return dependentTypeNames.values();
#else
    return dependentTypeNames.toList();
#endif
}

QList<Parser::Field> Generator::loggableFields(
    const QList<Parser::Field> & fields) const
{
    QList<Parser::Field> result;
    result.reserve(fields.size());

    for(const auto & field: fields)
    {
        if ( (field.m_name == QStringLiteral("authenticationToken")) ||
             (field.m_name == QStringLiteral("consumerKey")) ||
             (field.m_name == QStringLiteral("consumerSecret")) ||
             (field.m_name == QStringLiteral("password")) ||
             (field.m_name == QStringLiteral("oneTimeCode")) )
        {
            continue;
        }

        result.push_back(field);
    }

    return result;
}

bool Generator::structContainsLocalFields(const Parser::Structure & s) const
{
    for (const auto & f: qAsConst(s.m_fields))
    {
        if (f.m_affiliation == Parser::Field::Affiliation::Local) {
            return true;
        }
    }

    return false;
}

bool Generator::structContainsLocalFieldsRecursive(
    const Parser::Structure & s, const Parser::Structures & structs) const
{
    for (const auto & f: s.m_fields)
    {
        if (f.m_affiliation == Parser::Field::Affiliation::Local) {
            return true;
        }

        auto identifierType =
            std::dynamic_pointer_cast<Parser::IdentifierType>(f.m_type);

        if (!identifierType)
        {
            if (const auto listType =
                std::dynamic_pointer_cast<Parser::ListType>(f.m_type))
            {
                identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        listType->m_valueType);
            }
            else if (const auto setType =
                     std::dynamic_pointer_cast<Parser::SetType>(f.m_type))
            {
                identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        setType->m_valueType);
            }
            else if (const auto mapType =
                     std::dynamic_pointer_cast<Parser::MapType>(f.m_type))
            {
                identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        mapType->m_valueType);
            }
            else if (const auto hashType =
                     std::dynamic_pointer_cast<Parser::HashType>(f.m_type))
            {
                identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        hashType->m_valueType);
            }

            if (!identifierType) {
                continue;
            }
        }

        const auto actualType =
            aliasedTypeName(clearInclude(identifierType->m_identifier));

        const auto sit = std::find_if(
            structs.constBegin(),
            structs.constEnd(),
            [&actualType](const Parser::Structure & strct)
            {
                return strct.m_name == actualType;
            });

        if (sit == structs.constEnd()) {
            continue;
        }

        bool foundLocalField = false;
        for (const auto & f: qAsConst(sit->m_fields))
        {
            if (f.m_affiliation == Parser::Field::Affiliation::Local) {
                foundLocalField = true;
                break;
            }
        }

        if (foundLocalField) {
            return true;
        }

        if (structContainsLocalFieldsRecursive(*sit, structs)) {
            return true;
        }
    }

    return false;
}

bool Generator::structContainsLocalIdField(const Parser::Structure & s) const
{
    for (const auto & f: qAsConst(s.m_fields))
    {
        if ((f.m_affiliation == Parser::Field::Affiliation::Local) &&
            (f.m_name == QStringLiteral("localId")) &&
            dynamic_cast<Parser::StringType*>(f.m_type.get()))
        {
            return true;
        }
    }

    return false;
}

Parser::Structures Generator::collectStructsWithLocalFields(
    Parser & parser) const
{
    Parser::Structures relevantStructs;
    const auto & structs = parser.structures();
    for (const auto & s: qAsConst(structs))
    {
        if (structContainsLocalFieldsRecursive(s, structs)) {
            relevantStructs.push_back(s);
            continue;
        }
    }

    std::sort(
        relevantStructs.begin(),
        relevantStructs.end(),
        [](const Parser::Structure & lhs, const Parser::Structure & rhs)
        {
            return lhs.m_name.localeAwareCompare(rhs.m_name) < 0;
        });

    return relevantStructs;
}

std::optional<Parser::Structure> Generator::structForType(
    const std::shared_ptr<Parser::Type> & type, const Parser & parser) const
{
    if (!type) {
        return std::nullopt;
    }

    const auto identifierType =
        std::dynamic_pointer_cast<Parser::IdentifierType>(type);
    if (!identifierType) {
        return std::nullopt;
    }

    const auto actualType =
        aliasedTypeName(clearInclude(identifierType->m_identifier));

    const auto & structs = parser.structures();
    const auto sit = std::find_if(
        structs.constBegin(),
        structs.constEnd(),
        [&actualType](const Parser::Structure & strct)
        {
            return actualType == strct.m_name;
        });

    if (sit == structs.constEnd()) {
        return std::nullopt;
    }

    return *sit;
}

QString Generator::camelCaseToSnakeCase(const QString & input) const
{
    QString result;

    for(auto it = input.begin(), end = input.end(); it != end; ++it) {
        if (it->isUpper() && it != input.begin()) {
            result += QStringLiteral("_");
        }

        result += it->toLower();
    }

    return result;
}

void Generator::generateGetRandomValueExpression(
    const Parser::Field & field,
    const QString & prefix,
    const Parser & parser,
    QTextStream & out,
    const QString & end)
{
    if (dynamic_cast<Parser::PrimitiveType*>(field.m_type.get()) ||
        dynamic_cast<Parser::StringType*>(field.m_type.get()) ||
        dynamic_cast<Parser::ByteArrayType*>(field.m_type.get()) ||
        dynamic_cast<Parser::VariantType*>(field.m_type.get()))
    {
        out << prefix;
        if (!field.m_name.isEmpty()) {
            out << fieldSetterName(field) << "(";
        }

        out << getGenerateRandomValueFunction(
            typeToStr(field.m_type, {}, MethodType::TypeName));

        if (!field.m_name.isEmpty()) {
            out << ")";
        }

        out << end;
        return;
    }

    const auto identifierType =
        std::dynamic_pointer_cast<Parser::IdentifierType>(field.m_type);
    if (identifierType)
    {
        out << prefix;
        auto actualType = clearInclude(identifierType->m_identifier);
        actualType = aliasedTypeName(actualType);

        const auto & exceptions = parser.exceptions();
        auto exceptionIt = std::find_if(
            exceptions.begin(),
            exceptions.end(),
            [&] (const Parser::Structure & s)
            {
                return s.m_name == actualType;
            });

        const auto & enumerations = parser.enumerations();
        auto enumIt = std::find_if(
            enumerations.begin(),
            enumerations.end(),
            [&] (const Parser::Enumeration & e)
            {
                return e.m_name == actualType;
            });

        if (enumIt != enumerations.end())
        {
            const Parser::Enumeration & e = *enumIt;
            if (e.m_values.isEmpty()) {
                throw std::runtime_error(
                    "Detected enumeration without items: " +
                    e.m_name.toStdString());
            }

            int index = rand() % e.m_values.size();

            if (!field.m_name.isEmpty()) {
                out << fieldSetterName(field) << "(";
            }

            out << actualType << "::" << e.m_values[index].first;

            if (!field.m_name.isEmpty()) {
                out << ")";
            }

            out << end;
        }
        else if (exceptionIt != exceptions.end())
        {
            if (field.m_name.isEmpty()) {
                throw std::runtime_error{
                    "Generation of random exceptions is only supported for "
                    "struct fields"};
            }

            out << fieldSetterName(field) << "("
                << actualType << "());" << ln;

            const QString fieldPrefix = prefix + QStringLiteral("mutable") +
                capitalize(field.m_name) + QStringLiteral("()") +
                (field.m_required == Parser::Field::RequiredFlag::Optional
                 ? QStringLiteral("->")
                 : QStringLiteral("."));

            for(const auto & f: exceptionIt->m_fields) {
                generateGetRandomValueExpression(f, fieldPrefix, parser, out);
            }
        }
        else
        {
            if (!field.m_name.isEmpty()) {
                out << fieldSetterName(field) << "(";
            }

            out << getGenerateRandomValueFunction(actualType);

            if (!field.m_name.isEmpty()) {
                out << ")";
            }

            out << end;
        }


        return;
    }

    const auto listType =
        std::dynamic_pointer_cast<Parser::ListType>(field.m_type);
    if (listType)
    {
        verifyTypeIsValueOrIdentifier(listType->m_valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto valueType = typeToStr(
                listType->m_valueType,
                {},
                MethodType::TypeName);

            out << prefix << fieldSetterName(field) << "(QList<"
                << valueType << ">());" << ln;
        }

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << "mutable" << capitalize(field.m_name) << "()";

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                out << "->";
            }
            else {
                out << ".";
            }

            out << "push_back(";

            Parser::Field pseudoField;
            pseudoField.m_type = listType->m_valueType;
            pseudoField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(
                pseudoField, {}, parser, out, QStringLiteral(");\n"));
        }

        return;
    }

    const auto setType =
        std::dynamic_pointer_cast<Parser::SetType>(field.m_type);
    if (setType)
    {
        verifyTypeIsValueOrIdentifier(setType->m_valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto valueType = typeToStr(
                setType->m_valueType,
                {},
                MethodType::TypeName);

            out << prefix << fieldSetterName(field) << "(QSet<"
                << valueType << ">());" << ln;
        }

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << "mutable" << capitalize(field.m_name) << "()";

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                out << "->";
            }
            else {
                out << ".";
            }

            out << "insert(";

            Parser::Field pseudoField;
            pseudoField.m_type = setType->m_valueType;
            pseudoField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(
                pseudoField, {}, parser, out, QStringLiteral(");\n"));
        }

        return;
    }

    const auto mapType =
        std::dynamic_pointer_cast<Parser::MapType>(field.m_type);

    const auto hashType =
        std::dynamic_pointer_cast<Parser::HashType>(field.m_type);

    if (mapType || hashType)
    {
        const auto & keyType =
            (mapType ? mapType->m_keyType : hashType->m_keyType);

        const auto & valueType =
            (mapType ? mapType->m_valueType : hashType->m_valueType);

        verifyTypeIsValueOrIdentifier(keyType);
        verifyTypeIsValueOrIdentifier(valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto keyTypeName = typeToStr(
                keyType,
                {},
                MethodType::TypeName);

            const auto valueTypeName = typeToStr(
                valueType,
                {},
                MethodType::TypeName);

            out << prefix << fieldSetterName(field) << "(";
            if (mapType) {
                out << "QMap";
            }
            else {
                out << "QHash";
            }

            out << "<"
                << keyTypeName << ", " << valueTypeName << ">());" << ln;
        }

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << "mutable" << capitalize(field.m_name) << "()";

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                out << "->";
            }
            else {
                out << ".";
            }

            out << "insert(";

            Parser::Field pseudoKeyField;
            pseudoKeyField.m_type = keyType;
            pseudoKeyField.m_required = Parser::Field::RequiredFlag::Required;
            pseudoKeyField.m_name.clear();

            generateGetRandomValueExpression(
                pseudoKeyField, {}, parser, out, {});

            out << ", ";

            Parser::Field pseudoValueField;
            pseudoValueField.m_type = valueType;
            pseudoValueField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(
                pseudoValueField, {}, parser, out, QStringLiteral(");\n"));
        }

        return;
    }

    throw std::runtime_error(
        "Unsupported field type: " +
        typeToStr(field.m_type, {}, MethodType::TypeName).toStdString());
}

void Generator::verifyTypeIsValueOrIdentifier(
    const std::shared_ptr<Parser::Type> & type) const
{
    if (!dynamic_cast<Parser::PrimitiveType*>(type.get()) &&
        !dynamic_cast<Parser::StringType*>(type.get()) &&
        !dynamic_cast<Parser::ByteArrayType*>(type.get()) &&
        !dynamic_cast<Parser::VariantType*>(type.get()) &&
        !dynamic_cast<Parser::IdentifierType*>(type.get()))
    {
        auto typeName = typeToStr(type, {}, MethodType::TypeName);
        throw std::runtime_error(
            "Unsupported type: expecting base or identifier type: " +
            typeName.toStdString());
    }
}

QString Generator::getGenerateRandomValueFunction(const QString & typeName) const
{
    if (typeName == QStringLiteral("bool"))
    {
        return QStringLiteral("generateRandomBool()");
    }
    else if (typeName == QStringLiteral("QString") ||
             typeName == QStringLiteral("QVariant"))
    {
        return QStringLiteral("generateRandomString()");
    }
    else if (typeName == QStringLiteral("double"))
    {
        return QStringLiteral("generateRandomDouble()");
    }
    else if (typeName == QStringLiteral("QByteArray"))
    {
        return QStringLiteral("generateRandomString().toUtf8()");
    }
    else if (typeName == QStringLiteral("char"))
    {
        return QStringLiteral("generateRandomUint8()");
    }
    else if (typeName == QStringLiteral("qint16"))
    {
        return QStringLiteral("generateRandomInt16()");
    }
    else if (typeName == QStringLiteral("qint32"))
    {
        return QStringLiteral("generateRandomInt32()");
    }
    else if (typeName == QStringLiteral("qint64"))
    {
        return QStringLiteral("generateRandomInt64()");
    }
    else {
        return QStringLiteral("generateRandom") + typeName + QStringLiteral("()");
    }
}

QString Generator::fieldTypeToStr(const Parser::Field & field) const
{
    QString fieldTypeName = typeToStr(field.m_type, field.m_name);
    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        fieldTypeName = QStringLiteral("std::optional<") + fieldTypeName +
            QStringLiteral(">");
    }

    return fieldTypeName;
}

bool Generator::isFieldOfPrimitiveType(
    const Parser::Field & field, const QString & fieldTypeName) const
{
    bool isPrimitiveType = false;
    if (field.m_required != Parser::Field::RequiredFlag::Optional)
    {
        if (m_allEnums.contains(fieldTypeName)) {
            isPrimitiveType = true;
        }
        else if (m_primitiveTypeAliases.contains(fieldTypeName)) {
            isPrimitiveType = true;
        }
        else if (dynamic_cast<Parser::PrimitiveType*>(field.m_type.get())) {
            isPrimitiveType = true;
        }
    }

    return isPrimitiveType;
}

void Generator::writeTypeProperties(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    QHash<QString, QString> typeAliases;
    for(const auto & f: s.m_fields)
    {
        const auto fieldTypeName = typeToStr(f.m_type);
        if (fieldTypeName.contains(QStringLiteral(","))) {
            // In earlier versions of Qt Q_PROPERTY macro can't handle type
            // names containing comma
            typeAliases[fieldTypeName] = capitalize(f.m_name);
        }
    }

    for(auto it = typeAliases.constBegin(), end = typeAliases.constEnd();
        it != end; ++it)
    {
        ctx.m_out << indent << "using " << it.value() << " = "
            << it.key() << ";" << ln;
    }

    if (!typeAliases.isEmpty()) {
        ctx.m_out << ln;
    }

    for(const auto & f: s.m_fields)
    {
        auto fieldTypeName = typeToStr( f.m_type);
        auto it = typeAliases.find(fieldTypeName);
        if (it != typeAliases.end()) {
            fieldTypeName = it.value();
        }

        if (f.m_required == Parser::Field::RequiredFlag::Optional) {
            fieldTypeName = QStringLiteral("std::optional<") +
                fieldTypeName + QStringLiteral(">");
        }

        ctx.m_out << indent << "Q_PROPERTY(" << fieldTypeName
            << " " << fieldPropertyName(f) << " READ " << f.m_name
            << " WRITE " << fieldSetterName(f) << ")" << ln;
    }
}

void Generator::writeNamespaceBegin(OutputFileContext & ctx)
{
    ctx.m_out << "namespace qevercloud {" << ln << ln;
}

void Generator::writeNamespaceEnd(QTextStream & out)
{
    out << "} // namespace qevercloud" << ln;
}

void Generator::writeEnumeration(
    OutputFileContext & ctx, const Parser::Enumeration & e) const
{
    if (!e.m_docComment.isEmpty()) {
        ctx.m_out << e.m_docComment << ln;
    }

    ctx.m_out << "enum class " << e.m_name << ln << "{" << ln;

    size_t i = 0;
    size_t numValues = e.m_values.size();
    for(const auto & v: e.m_values)
    {
        ctx.m_out << "    " << v.first;

        if (!v.second.isEmpty()) {
            ctx.m_out << " = " << v.second;
        }

        if (i < (numValues - 1)) {
            ctx.m_out << ",";
        }

        ctx.m_out << ln;
        ++i;
    }

    ctx.m_out << "};" << ln << ln;

    if (ctx.m_type == OutputFileType::Interface) {
        ctx.m_out << "#if QT_VERSION >= QT_VERSION_CHECK(5, 8, 0)" << ln
            << "#if QEVERCLOUD_USES_Q_NAMESPACE" << ln
            << "Q_ENUM_NS(" << e.m_name << ")" << ln
            << "#endif" << ln
            << "#endif" << ln << ln;
    }

    ctx.m_out << "inline uint qHash(" << e.m_name << " value)"
        << ln
        << "{" << ln
        << "    return static_cast<uint>(value);" << ln
        << "}" << ln << ln;
}

void Generator::writeEnumerationPrintDeclaration(
    QTextStream & out, const Parser::Enumeration & e,
    const char * printer) const
{
    out << "QEVERCLOUD_EXPORT " << printer << " & operator<<(" << ln
        << "    " << printer << " & out, const " << e.m_name << " value);"
        << ln << ln;
}

void Generator::writeEnumerationPrintDefinition(
    QTextStream & out, const Parser::Enumeration & e,
    const char * printer) const
{
    out << printer << " & operator<<(" << ln
        << "    " << printer << " & out, const "
        << e.m_name << " value)" << ln << "{" << ln
        << "    switch(value)" << ln
        << "    {" << ln;

    for(const auto & value: e.m_values) {
        out << "    case " << e.m_name << "::" << value.first << ":" << ln
            << "        out << \"" << e.m_name << "::" << value.first << "\";"
            << ln << "        break;" << ln;
    }

    out << "    default:" << ln
        << "        out << \"Unknown (\" << static_cast<qint64>(value) << \")\";"
        << ln << "        break;" << ln
        << "    }" << ln
        << "    return out;" << ln
        << "}" << ln << ln;
}

void Generator::writeTypeImplPrintDefinition(
    QTextStream & out, const Parser::Structure & s) const
{
    out << "void " << s.m_name
        << "::Impl::print(QTextStream & strm) const" << ln
        << "{" << ln;

    out << indent << "strm << \"" << s.m_name << ": {\\n\";" << ln;

    bool previousOptional = false;
    for(const auto & f: s.m_fields)
    {
        const auto listType =
            std::dynamic_pointer_cast<Parser::ListType>(f.m_type);

        const auto setType =
            std::dynamic_pointer_cast<Parser::SetType>(f.m_type);

        const auto mapType =
            std::dynamic_pointer_cast<Parser::MapType>(f.m_type);

        const auto hashType =
            std::dynamic_pointer_cast<Parser::HashType>(f.m_type);

        if (f.m_required == Parser::Field::RequiredFlag::Optional)
        {
            if (!previousOptional) {
                out << ln;
            }

            out << indent << "if (m_" << f.m_name << ") {" << ln
                << indent << indent << "strm << \"" << indent << f.m_name
                << " = \"";

            if (mapType || hashType)
            {
                const auto & keyType =
                    (mapType ? mapType->m_keyType : hashType->m_keyType);

                const auto & valueType =
                    (mapType ? mapType->m_valueType : hashType->m_valueType);

                out << ln << indent << indent << indent << "<< \"";
                if (mapType) {
                    out << "QMap";
                }
                else {
                    out << "QHash";
                }

                const auto valueTypeName = typeToStr(valueType, {});

                out << "<"
                    << typeToStr(keyType, {}) << ", "
                    << valueTypeName
                    << "> {\";" << ln
                    << indent << indent << "for(const auto & it: toRange(*m_"
                    << f.m_name << ")) {" << ln
                    << indent << indent << indent << "strm << \""
                    << indent << indent << "[\" << it.key() << \"] = \"";

                if (valueTypeName == QStringLiteral("QVariant"))
                {
                    out << ";" << ln;
                    out << indent << indent << indent << "QString debugStr;"
                        << ln;

                    out << indent << indent << indent
                        << "QDebug dbg{&debugStr};" << ln;

                    out << indent << indent << indent << "dbg << it.value();"
                        << ln;

                    out << indent << indent << indent
                        << "strm << debugStr << \"\\n\";" << ln;
                }
                else
                {
                    out << " << it.value() << \"\\n\";" << ln;
                }

                out << indent << indent << "}" << ln
                    << indent << indent << "strm << \"    }\\n\";" << ln;
            }
            else if (setType)
            {
                out << ln << indent << indent << indent << "<< \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << ln
                    << indent << indent << "for(const auto & v: *m_" << f.m_name
                    << ") {" << ln
                    << indent << indent << indent << "strm << \""
                    << indent << indent << "\" << v << \"\\n\";" << ln
                    << indent << indent << "}" << ln
                    << indent << indent << "strm << \"    }\\n\";" << ln;
            }
            else if (listType)
            {
                out << ln << indent << indent << indent << "<< \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << ln
                    << indent << indent << "for(const auto & v: *m_" << f.m_name
                    << ") {" << ln
                    << indent << indent << indent << "strm << \""
                    << indent << indent << "\" << v << \"\\n\";" << ln
                    << indent << indent << "}" << ln
                    << indent << indent << "strm << \"    }\\n\";" << ln;
            }
            else if (s.m_name == QStringLiteral("Data") &&
                     f.m_name == QStringLiteral("body"))
            {
                out << ";" << ln
                    << indent << indent << "if (m_" << f.m_name
                    << "->size() <= 1024) {" << ln
                    << indent << indent << indent << "strm << m_"
                    << f.m_name << "->toHex() << \"\\n\";" << ln
                    << indent << indent << "}" << ln
                    << indent << indent << "else {" << ln
                    << indent << indent << indent
                    << "strm << \"<binary data, \" << m_" << f.m_name
                    << "->size() << \" bytes>\" << \"\\n\";" << ln
                    << indent << indent << "}" << ln;
            }
            else if (dynamic_cast<Parser::ByteArrayType*>(f.m_type.get()) &&
                     f.m_name.endsWith(QStringLiteral("Hash")))
            {
                out << ln << indent << indent << indent << "<< m_"
                    << f.m_name << "->toHex() << \"\\n\";" << ln;
            }
            else if (const auto * primitiveType =
                     dynamic_cast<Parser::PrimitiveType*>(f.m_type.get());
                     primitiveType &&
                     primitiveType->m_type == Parser::PrimitiveType::Type::Bool)
            {
                out << ln << indent << indent << indent << "<< (*m_"
                    << f.m_name << " ? \"true\" : \"false\") << \"\\n\";" << ln;
            }
            else if (const auto * variantType =
                     dynamic_cast<Parser::VariantType*>(f.m_type.get()))
            {
                out << ";" << ln;
                out << indent << indent << "QString debugStr;" << ln;
                out << indent << indent << "QDebug dbg{&debugStr};" << ln;
                out << indent << indent << "dbg << *m_" << f.m_name << ";"
                    << ln;
                out << indent << indent << "strm << debugStr;" << ln;
            }
            else
            {
                out << ln << indent << indent << indent << "<< *m_"
                    << f.m_name << " << \"\\n\";" << ln;
            }

            out << indent << "}" << ln
                << indent << "else {" << ln
                << indent << indent << "strm << \"" << indent << f.m_name
                << " is not set\\n\";" << ln
                << indent << "}" << ln << ln;
            previousOptional = true;
        }
        else
        {
            out << indent << "strm << \"" << indent << f.m_name << " = \""
                << ln;

            if (mapType || hashType)
            {
                const auto & keyType =
                    (mapType ? mapType->m_keyType : hashType->m_keyType);

                const auto & valueType =
                    (mapType ? mapType->m_valueType : hashType->m_valueType);

                out << indent << indent << "<< \"";
                if (mapType) {
                    out << "QMap";
                }
                else {
                    out << "QHash";
                }

                const auto valueTypeName = typeToStr(valueType, {});

                out << "<"
                    << typeToStr(keyType, {}) << ", "
                    << valueTypeName
                    << "> {\";" << ln
                    << indent << "for(const auto & it: toRange(m_"
                    << f.m_name << ")) {" << ln
                    << indent << indent << "strm << \""
                    << indent << "[\" << it.key() << \"] = \"";

                if (valueTypeName == QStringLiteral("QVariant"))
                {
                    out << ";" << ln;
                    out << indent << indent << "QString debugStr;"
                        << ln;

                    out << indent << indent
                        << "QDebug dbg{&debugStr};" << ln;

                    out << indent << indent << "dbg << it.value();"
                        << ln;

                    out << indent << indent
                        << "strm << debugStr << \"\\n\";" << ln;
                }
                else
                {
                    out << " << it.value() << \"\\n\";" << ln;
                }

                out << indent << "}" << ln
                    << indent << "strm << \"    }\\n\";" << ln;
            }
            else if (setType)
            {
                out << indent << indent << "<< \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << ln
                    << indent << "for(const auto & v: m_" << f.m_name << ") {"
                    << ln
                    << indent << indent << "strm << \"" << indent
                    << "\" << v << \"\\n\";" << ln
                    << indent << "}" << ln
                    << indent << "strm << \"}\\n\";" << ln;
            }
            else if (listType)
            {
                out << indent << indent << "<< \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << ln
                    << indent << "for(const auto & v: m_" << f.m_name << ") {"
                    << ln
                    << indent << indent << "strm << \"" << indent
                    << "\" << v << \"\\n\";" << ln
                    << indent << "}" << ln
                    << indent << "strm << \"}\\n\";" << ln;
            }
            else if (const auto * primitiveType =
                     dynamic_cast<Parser::PrimitiveType*>(f.m_type.get());
                     primitiveType &&
                     primitiveType->m_type == Parser::PrimitiveType::Type::Bool)
            {
                out << indent << indent << "<< (m_"
                    << f.m_name << " ? \"true\" : \"false\") << \"\\n\";" << ln;
            }
            else if (const auto * variantType =
                     dynamic_cast<Parser::VariantType*>(f.m_type.get()))
            {
                out << ";" << ln;
                out << indent << indent << "QString debugStr;" << ln;
                out << indent << indent << "QDebug dbg{&debugStr};" << ln;
                out << indent << indent << "dbg << m_" << f.m_name << ";"
                    << ln;
                out << indent << indent << "strm << debugStr;" << ln;
            }
            else
            {
                out << indent << indent << "<< m_"
                    << f.m_name << " << \"\\n\";" << ln;
            }

            previousOptional = false;
        }
    }

    out << indent << "strm << \"}\\n\";" << ln
        << "}" << ln << ln;
}

void Generator::generateTestServerHelperClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & func: service.m_functions)
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << blockSeparator << ln << ln;

        QString funcName = capitalize(func.m_name);

        ctx.m_out << "class " << service.m_name << funcName
            << "TesterHelper: public QObject" << ln
            << "{" << ln
            << "    Q_OBJECT" << ln;

        auto responseType = typeToStr(func.m_type, func.m_name);
        ctx.m_out << "public:" << ln
            << "    using Executor = std::function<" << ln
            << "        " << responseType << "(" << ln;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            auto paramType = typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << "            " << paramType << "," << ln;
        }

        ctx.m_out << "            IRequestContextPtr ctx)>;"
            << ln << ln;

        ctx.m_out << "public:" << ln
            << "    explicit " << service.m_name << funcName << "TesterHelper("
            << ln
            << "            Executor executor," << ln
            << "            QObject * parent = nullptr) :"
            << ln
            << "        QObject(parent)," << ln
            << "        m_executor(std::move(executor))" << ln
            << "    {}" << ln << ln;

        ctx.m_out << "Q_SIGNALS:" << ln
            << "    void " << func.m_name << "RequestReady(" << ln;
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        " << responseType << " value," << ln;
        }
        ctx.m_out << "        std::exception_ptr e);" << ln << ln;

        ctx.m_out << "public Q_SLOTS:" << ln
            << "    void on" << funcName << "RequestReceived(" << ln;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            auto paramType = typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            if (paramType.startsWith(QStringLiteral("const "))) {
                paramType = paramType.mid(6);
            }
            if (paramType.endsWith(QStringLiteral(" &"))) {
                paramType = paramType.mid(0, paramType.size() - 2);
            }

            ctx.m_out << "        " << paramType << " " << param.m_name
                << "," << ln;
        }

        ctx.m_out << "        IRequestContextPtr ctx)" << ln
            << "    {" << ln;

        ctx.m_out << "        try" << ln
            << "        {" << ln;

        ctx.m_out << "            ";

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "auto v = ";
        }

        ctx.m_out << "m_executor(" << ln;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << ln;
        }

        ctx.m_out << "                ctx);" << ln << ln;

        ctx.m_out << "            Q_EMIT " << func.m_name << "RequestReady("
            << ln;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "                v," << ln;
        }

        ctx.m_out << "                "
            << "std::exception_ptr{});" << ln
            << "        }" << ln;

        ctx.m_out << "        catch(const std::exception &)" << ln
            << "        {" << ln;

        ctx.m_out << "            Q_EMIT " << func.m_name << "RequestReady("
            << ln;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "                {}," << ln;
        }

        ctx.m_out << "                "
            << "std::current_exception());" << ln
            << "        }" << ln;

        ctx.m_out << "    }" << ln << ln;

        ctx.m_out << "private:" << ln
            << "    Executor m_executor;" << ln;

        ctx.m_out << "};" << ln << ln;
    }
}

void Generator::generateTestServerPrepareRequestParams(
    const Parser::Function & func,
    const QList<Parser::Enumeration> & enumerations,
    OutputFileContext & ctx)
{
    bool hasAuthenticationToken = false;
    for(const auto & param: func.m_params)
    {
        if (param.m_name == QStringLiteral("authenticationToken")) {
            // Auth token is a part of IRequestContext interface
            hasAuthenticationToken = true;
            continue;
        }

        const auto paramTypeName = typeToStr(
            param.m_type,
            {},
            MethodType::TypeName);

        QString actualParamTypeName;

        if (dynamic_cast<Parser::PrimitiveType*>(param.m_type.get()) ||
            dynamic_cast<Parser::StringType*>(param.m_type.get()) ||
            dynamic_cast<Parser::ByteArrayType*>(param.m_type.get()))
        {
            actualParamTypeName = paramTypeName;
        }
        else if (const auto * identifierType =
                 dynamic_cast<Parser::IdentifierType*>(param.m_type.get()))
        {
            actualParamTypeName = clearInclude(identifierType->m_identifier);
            actualParamTypeName = aliasedTypeName(actualParamTypeName);
        }
        else {
            throw std::runtime_error("Unsupported parameter type: " +
                                     paramTypeName.toStdString());
        }

        ctx.m_out << "    " << paramTypeName << " " << param.m_name
            << " = ";

        const auto enumIt = std::find_if(
            enumerations.begin(),
            enumerations.end(),
            [&] (const Parser::Enumeration & e)
            {
                return e.m_name == actualParamTypeName;
            });
        if (enumIt != enumerations.end())
        {
            const Parser::Enumeration & e = *enumIt;
            if (e.m_values.isEmpty()) {
                throw std::runtime_error(
                    "Detected enumeration without items: " +
                    e.m_name.toStdString());
            }

            int index = rand() % e.m_values.size();
            ctx.m_out << actualParamTypeName
                << "::" << e.m_values[index].first << ";" << ln;
        }
        else
        {
            ctx.m_out << getGenerateRandomValueFunction(actualParamTypeName)
                << ";" << ln;
        }
    }

    ctx.m_out << "    IRequestContextPtr ctx = newRequestContext(";
    if (hasAuthenticationToken) {
        ctx.m_out << ln
            << "        QStringLiteral(\"authenticationToken\")";
    }
    ctx.m_out << ");" << ln << ln;
}

void Generator::generateTestServerPrepareRequestResponse(
    const Parser::Function & func,
    const QList<Parser::Enumeration> & enumerations,
    OutputFileContext & ctx)
{
    QString responseTypeName = typeToStr(
        func.m_type,
        {},
        MethodType::TypeName);

    bool responseTypeIsVoid = (responseTypeName == QStringLiteral("void"));
    if (!responseTypeIsVoid) {
        ctx.m_out << "    " << responseTypeName << " response";
    }

    auto listType = std::dynamic_pointer_cast<Parser::ListType>(func.m_type);
    if (listType)
    {
        auto valueType = typeToStr(
            listType->m_valueType,
            {},
            MethodType::TypeName);

        auto actualValueType = clearInclude(valueType);
        actualValueType = aliasedTypeName(actualValueType);

        ctx.m_out << ";" << ln;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    response << "
                << getGenerateRandomValueFunction(actualValueType)
                << ";" << ln;
        }
        ctx.m_out << ln;

        return;
    }

    auto setType = std::dynamic_pointer_cast<Parser::SetType>(func.m_type);
    if (setType)
    {
        auto valueType = typeToStr(
            setType->m_valueType,
            {},
            MethodType::TypeName);

        auto actualValueType = clearInclude(valueType);
        actualValueType = aliasedTypeName(actualValueType);

        ctx.m_out << ";" << ln;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    Q_UNUSED(response.insert("
                << getGenerateRandomValueFunction(actualValueType)
                << "))" << ln;
        }
        ctx.m_out << ln;
        return;
    }

    auto mapType = std::dynamic_pointer_cast<Parser::MapType>(func.m_type);
    if (mapType)
    {
        auto keyType = typeToStr(
            mapType->m_keyType,
            {},
            MethodType::TypeName);

        auto valueType = typeToStr(
            mapType->m_valueType,
            {},
            MethodType::TypeName);

        auto actualKeyType = clearInclude(keyType);
        actualKeyType = aliasedTypeName(actualKeyType);

        auto actualValueType = clearInclude(valueType);
        actualValueType = aliasedTypeName(actualValueType);

        ctx.m_out << ";" << ln;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    response["
                << getGenerateRandomValueFunction(actualKeyType)
                << "] = "
                << getGenerateRandomValueFunction(actualValueType)
                << ";" << ln;
        }
        ctx.m_out << ln;
        return;
    }
    else if (!responseTypeIsVoid)
    {
        QString actualResponseTypeName = clearInclude(responseTypeName);
        actualResponseTypeName = aliasedTypeName(actualResponseTypeName);

        auto enumIt = std::find_if(
            enumerations.begin(),
            enumerations.end(),
            [&] (const Parser::Enumeration & e)
            {
                return e.m_name == actualResponseTypeName;
            });
        if (enumIt != enumerations.end())
        {
            const Parser::Enumeration & e = *enumIt;
            if (e.m_values.isEmpty()) {
                throw std::runtime_error(
                    "Detected enumeration without items: " +
                    e.m_name.toStdString());
            }

            int index = rand() % e.m_values.size();
            ctx.m_out << " = " << actualResponseTypeName
                << "::" << e.m_values[index].first << ";" << ln << ln;
        }
        else
        {
            ctx.m_out << " = "
                << getGenerateRandomValueFunction(actualResponseTypeName)
                << ";" << ln << ln;
        }
    }
}

void Generator::generateTestServerPrepareRequestExceptionResponse(
    const Parser & parser,
    const Parser::Field & e,
    OutputFileContext & ctx)
{
    auto exceptionTypeName = typeToStr(
        e.m_type,
        {},
        MethodType::TypeName);

    if (exceptionTypeName == QStringLiteral("ThriftException")) {
        ctx.m_out << "    auto " << e.m_name << " = "
            << "ThriftException(" << ln
            << "        ThriftException::Type::INTERNAL_ERROR," << ln
            << "        QStringLiteral(\"Internal error\"));" << ln;

        ctx.m_out << ln;
        return;
    }

    const auto & exceptions = parser.exceptions();
    auto exceptionIt = std::find_if(
        exceptions.begin(),
        exceptions.end(),
        [&] (const Parser::Structure & s)
        {
            return s.m_name == exceptionTypeName;
        });

    if (Q_UNLIKELY(exceptionIt == exceptions.end())) {
        throw std::runtime_error(
            "Failed to find exception by type name: " +
            exceptionTypeName.toStdString());
    }

    ctx.m_out << "    auto " << e.m_name << " = " << exceptionTypeName
        << "();" << ln;

    const QString fieldPrefix =
        QStringLiteral("    ") + e.m_name + QStringLiteral(".");

    for(const auto & f: exceptionIt->m_fields) {
        generateGetRandomValueExpression(f, fieldPrefix, parser, ctx.m_out);
    }

    ctx.m_out << ln;
}

void Generator::generateTestServerHelperLambda(
    const Parser::Service & service,
    const Parser::Function & func,
    const Parser & parser,
    OutputFileContext & ctx,
    const QString & exceptionToThrow)
{
    ctx.m_out << "    " << service.m_name << capitalize(func.m_name)
        << "TesterHelper helper(" << ln
        << "        [&] (";

    bool hasParams = false;
    bool firstParam = true;
    for(const auto & param: func.m_params)
    {
        if (param.m_name == QStringLiteral("authenticationToken")) {
            // Auth token is a part of IRequestContext interface
            continue;
        }

        hasParams = true;

        auto paramTypeName = typeToStr(
            param.m_type,
            {},
            MethodType::TypeName);

        if (!dynamic_cast<Parser::PrimitiveType*>(param.m_type.get()) &&
            !dynamic_cast<Parser::ByteArrayType*>(param.m_type.get()))
        {
            paramTypeName.prepend(QStringLiteral("const "));
            paramTypeName.append(QStringLiteral(" &"));
        }

        if (!firstParam) {
            ctx.m_out << "             ";
        }

        ctx.m_out << paramTypeName << " "
            << param.m_name << "Param," << ln;

        firstParam = false;
    }

    if (hasParams) {
        ctx.m_out << "             ";
    }

    auto returnTypeName = typeToStr(
        func.m_type,
        {},
        MethodType::TypeName);

    ctx.m_out << "IRequestContextPtr ctxParam) -> " << returnTypeName << ln;

    ctx.m_out << "        {" << ln;

    for(const auto & param: func.m_params)
    {
        if (param.m_name == QStringLiteral("authenticationToken")) {
            ctx.m_out << "            Q_ASSERT("
                << "ctx->authenticationToken() == "
                << "ctxParam->authenticationToken());" << ln;
            continue;
        }

        if (const auto s = structForType(param.m_type, parser); s &&
            structContainsLocalFieldsRecursive(*s, parser.structures()))
        {
            ctx.m_out << indent << indent << indent
                << "compareValuesWithoutLocalFields("
                << param.m_name << "Param, " << param.m_name << ");" << ln;
        }
        else if (const auto listType =
                    std::dynamic_pointer_cast<Parser::ListType>(param.m_type))
        {
            if (const auto s =
                structForType(listType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent << indent << indent
                    << "compareListValuesWithoutLocalFields("
                    << param.m_name << "Param, " << param.m_name << ");" << ln;
            }
            else
            {
                ctx.m_out << indent << indent << indent << "Q_ASSERT("
                    << param.m_name << " == " << param.m_name << "Param);"
                    << ln;
            }
        }
        else if (const auto setType =
                 std::dynamic_pointer_cast<Parser::SetType>(param.m_type))
        {
            if (const auto s =
                structForType(setType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent << indent << indent
                    << "compareSetValuesWithoutLocalFields("
                    << param.m_name << "Param, " << param.m_name << ");" << ln;
            }
            else
            {
                ctx.m_out << indent << indent << indent << "Q_ASSERT("
                    << param.m_name << " == " << param.m_name << "Param);"
                    << ln;
            }
        }
        else if (const auto mapType =
                 std::dynamic_pointer_cast<Parser::MapType>(param.m_type))
        {
            if (const auto s =
                structForType(setType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent << indent << indent
                    << "compareMapValuesWithoutLocalFields("
                    << param.m_name << "Param, " << param.m_name << ");" << ln;
            }
            else
            {
                ctx.m_out << indent << indent << indent << "Q_ASSERT("
                    << param.m_name << " == " << param.m_name << "Param);"
                    << ln;
            }
        }
        else
        {
            ctx.m_out << indent << indent << indent << "Q_ASSERT("
                << param.m_name << " == " << param.m_name << "Param);" << ln;
        }
    }

    if (!exceptionToThrow.isEmpty()) {
        ctx.m_out << "            throw " << exceptionToThrow << ";" << ln
            << "        });" << ln << ln;
        return;
    }

    ctx.m_out << "            return";

    auto funcVoidType = std::dynamic_pointer_cast<Parser::VoidType>(func.m_type);
    if (!funcVoidType) {
        ctx.m_out << " response";
    }

    ctx.m_out << ";" << ln
        << "        });" << ln << ln;
}

void Generator::generateTestServerSocketSetup(
    const Parser::Service & service,
    const Parser::Function & func,
    OutputFileContext & ctx)
{
    auto funcName = capitalize(func.m_name);

    ctx.m_out << "    " << service.m_name << "Server server;" << ln
        << "    QObject::connect(" << ln
        << "        &server," << ln
        << "        &" << service.m_name << "Server::" << func.m_name
        << "Request," << ln
        << "        &helper," << ln
        << "        &" << service.m_name << funcName
        << "TesterHelper::on" << funcName << "RequestReceived);"
        << ln;

    ctx.m_out << "    QObject::connect(" << ln
        << "        &helper," << ln
        << "        &" << service.m_name << funcName << "TesterHelper::"
        << func.m_name << "RequestReady," << ln
        << "        &server," << ln
        << "        &" << service.m_name << "Server::on" << funcName
        << "RequestReady);" << ln << ln;

    ctx.m_out << "    QTcpServer tcpServer;" << ln
        << "    QVERIFY(tcpServer.listen(QHostAddress::LocalHost));"
        << ln
        << "    quint16 port = tcpServer.serverPort();" << ln
        << ln;

    ctx.m_out << "    QTcpSocket * pSocket = nullptr;" << ln
        << "    QObject::connect(" << ln
        << "        &tcpServer," << ln
        << "        &QTcpServer::newConnection," << ln
        << "        &tcpServer," << ln
        << "        [&] {" << ln
        << "            pSocket = tcpServer.nextPendingConnection();"
        << ln
        << "            Q_ASSERT(pSocket);" << ln
        << "            QObject::connect(" << ln
        << "                pSocket," << ln
        << "                &QAbstractSocket::disconnected," << ln
        << "                pSocket," << ln
        << "                &QAbstractSocket::deleteLater);" << ln
        << "            if (!pSocket->waitForConnected()) {" << ln
        << "                QFAIL(\"Failed to establish connection\");"
        << ln
        << "            }" << ln << ln
        << "            QByteArray requestData = "
        << "readThriftRequestFromSocket(*pSocket);" << ln
        << "            server.onRequest(requestData);" << ln
        << "        });" << ln << ln;

    ctx.m_out << "    QObject::connect(" << ln
        << "        &server," << ln
        << "        &" << service.m_name << "Server::" << func.m_name
        << "RequestReady," << ln
        << "        &server," << ln
        << "        [&] (QByteArray responseData)" << ln
        << "        {" << ln
        << "            QByteArray buffer;" << ln
        << "            buffer.append(\"HTTP/1.1 200 OK\\r\\n\");"
        << ln
        << "            buffer.append(\"Content-Length: \");" << ln
        << "            buffer.append(QString::number("
        << "responseData.size()).toUtf8());" << ln
        << "            buffer.append(\"\\r\\n\");" << ln
        << "            buffer.append(\"Content-Type: "
        << "application/x-thrift\\r\\n\\r\\n\");" << ln
        << "            buffer.append(responseData);" << ln << ln
        << "            if (!writeBufferToSocket(buffer, "
        << "*pSocket)) {" << ln
        << "                QFAIL(\"Failed to write response to socket\");"
        << ln
        << "            }" << ln
        << "        });" << ln << ln;
}

void Generator::generateTestServerServiceCall(
    Parser & parser,
    const Parser::Service & service,
    const Parser::Function & func,
    const ServiceCallKind callKind,
    OutputFileContext & ctx,
    const QString & exceptionTypeToCatch,
    const QString & exceptionNameToCompare)
{
    auto funcReturnTypeName = typeToStr(
        func.m_type,
        {},
        MethodType::TypeName);

    auto serviceName = decapitalize(service.m_name);

    ctx.m_out << "    auto "
        << serviceName << " =" << ln
        << "        new" << service.m_name << "(" << ln
        << "            QStringLiteral(\"http://127.0.0.1:\") + "
        << "QString::number(port)," << ln;

    if (service.m_name == QStringLiteral("NoteStore")) {
        ctx.m_out << "            QString{}," << ln;
    }

    ctx.m_out << "            nullptr," << ln
        << "            nullRetryPolicy());" << ln << ln;

    QString indentStr = QStringLiteral("    ");
    if (!exceptionTypeToCatch.isEmpty())
    {
        ctx.m_out << indent << "bool caughtException = false;" << ln;

        ctx.m_out << indent << "try" << ln
            << indent << "{" << ln;

        indentStr += QString::fromUtf8(indent);
    }

    if (callKind == ServiceCallKind::Sync)
    {
        ctx.m_out << indentStr;
        if (funcReturnTypeName != QStringLiteral("void")) {
            ctx.m_out << funcReturnTypeName << " res = ";
        }

        ctx.m_out << serviceName << "->" << func.m_name << "(" << ln;
    }
    else if (callKind == ServiceCallKind::Async)
    {
        ctx.m_out << indentStr << "QFuture<" << funcReturnTypeName
            << "> result = " << serviceName << "->" << func.m_name << "Async("
            << ln;
    }
    else
    {
        throw std::runtime_error(
            "Unsupported service call kind: " +
            QString::number(static_cast<qint64>(callKind)).toStdString());
    }

    for(const auto & param: func.m_params)
    {
        if (param.m_name == QStringLiteral("authenticationToken")) {
            // Auth token is a part of IRequestContext interface
            continue;
        }

        ctx.m_out << indentStr << "    " << param.m_name << "," << ln;
    }

    ctx.m_out << indentStr << "    ctx);" << ln << ln;

    if (callKind == ServiceCallKind::Async)
    {
        ctx.m_out << indentStr << "QFutureWatcher<" << funcReturnTypeName
            << "> watcher;" << ln
            << indentStr << "QEventLoop loop;" << ln
            << indentStr << "QObject::connect(" << ln
            << indentStr << indent << "&watcher, "
            << "&QFutureWatcher<" << funcReturnTypeName << ">::finished, &loop,"
            << ln
            << indentStr << indent << "&QEventLoop::quit);" << ln << ln
            << indentStr << "watcher.setFuture(result);" << ln
            << indentStr << "loop.exec();" << ln;

        if (exceptionTypeToCatch.isEmpty())
        {
            if (funcReturnTypeName != QStringLiteral("void")) {
                ctx.m_out << ln;
                if (const auto s = structForType(func.m_type, parser); s &&
                    structContainsLocalFieldsRecursive(*s, parser.structures()))
                {
                    ctx.m_out << indentStr << "compareValuesWithoutLocalFields("
                        << "result.result(), response);" << ln;
                }
                else if (const auto listType =
                    std::dynamic_pointer_cast<Parser::ListType>(func.m_type))
                {
                    if (const auto s =
                        structForType(listType->m_valueType, parser); s &&
                        structContainsLocalFieldsRecursive(
                            *s, parser.structures()))
                    {
                        ctx.m_out << indentStr
                            << "compareListValuesWithoutLocalFields("
                            << "result.result(), response);" << ln;
                    }
                    else
                    {
                        ctx.m_out << indentStr
                            << "QVERIFY(result.result() == response);" << ln;
                    }
                }
                else if (const auto setType =
                         std::dynamic_pointer_cast<Parser::SetType>(
                             func.m_type))
                {
                    if (const auto s =
                        structForType(setType->m_valueType, parser); s &&
                        structContainsLocalFieldsRecursive(
                            *s, parser.structures()))
                    {
                        ctx.m_out << indentStr
                            << "compareSetValuesWithoutLocalFields("
                            << "result.result(), response);" << ln;
                    }
                    else
                    {
                        ctx.m_out << indentStr
                            << "QVERIFY(result.result() == response);" << ln;
                    }
                }
                else if (const auto mapType =
                    std::dynamic_pointer_cast<Parser::MapType>(func.m_type))
                {
                    if (const auto s =
                        structForType(mapType->m_valueType, parser); s &&
                        structContainsLocalFieldsRecursive(
                            *s, parser.structures()))
                    {
                        ctx.m_out << indentStr
                            << "compareMapValuesWithoutLocalFields("
                            << "result.result(), response);" << ln;
                    }
                    else
                    {
                        ctx.m_out << indentStr
                            << "QVERIFY(result.result() == response);" << ln;
                    }
                }
                else
                {
                    ctx.m_out << indentStr
                        << "QVERIFY(result.result() == response);" << ln;
                }
            }
        }
        else
        {
            ctx.m_out << ln;
            ctx.m_out << indentStr << "result.waitForFinished();" << ln;
        }
    }

    if (!exceptionTypeToCatch.isEmpty())
    {
        if ((callKind == ServiceCallKind::Sync) &&
            (funcReturnTypeName != QStringLiteral("void")))
        {
            ctx.m_out << indentStr << "Q_UNUSED(res)" << ln;
        }

        ctx.m_out << "    }" << ln
            << "    catch(const " << exceptionTypeToCatch << " & e)" << ln
            << "    {" << ln
            << "        caughtException = true;" << ln
            << "        QVERIFY(e == " << exceptionNameToCompare << ");" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    QVERIFY(caughtException);" << ln;
        return;
    }

    if ((callKind == ServiceCallKind::Sync) &&
        (funcReturnTypeName != QStringLiteral("void")))
    {
        if (const auto s = structForType(func.m_type, parser); s &&
            structContainsLocalFieldsRecursive(*s, parser.structures()))
        {
            ctx.m_out << indent
                << "compareValuesWithoutLocalFields(res, response);" << ln;
        }
        else if (const auto listType =
                 std::dynamic_pointer_cast<Parser::ListType>(func.m_type))
        {
            if (const auto s =
                structForType(listType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent
                    << "compareListValuesWithoutLocalFields(res, response);" << ln;
            }
            else
            {
                ctx.m_out << indent
                    << "QVERIFY(res == response);" << ln;
            }
        }
        else if (const auto setType =
                 std::dynamic_pointer_cast<Parser::SetType>(func.m_type))
        {
            if (const auto s =
                structForType(setType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent
                    << "compareSetValuesWithoutLocalFields(res, response);" << ln;
            }
            else
            {
                ctx.m_out << indent
                    << "QVERIFY(res == response);" << ln;
            }
        }
        else if (const auto mapType =
                 std::dynamic_pointer_cast<Parser::MapType>(func.m_type))
        {
            if (const auto s =
                structForType(mapType->m_valueType, parser); s &&
                structContainsLocalFieldsRecursive(*s, parser.structures()))
            {
                ctx.m_out << indent
                    << "compareMapValuesWithoutLocalFields(res, response);"
                    << ln;
            }
            else
            {
                ctx.m_out << indent
                    << "QVERIFY(res == response);" << ln;
            }
        }
        else
        {
            ctx.m_out << indent
                << "QVERIFY(res == response);" << ln;
        }
    }
}

void Generator::writeHeaderHeader(
    OutputFileContext & ctx, const QString & fileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind,
    const QString & section,
    const QStringList & forwardDeclarationsOutsideNamespace)
{
    ctx.m_out << disclaimer << ln;

    const QString guard = getIncludeGuard(fileName, section);

    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include <qevercloud/Export.h>" << ln << ln;
    }

    for(const auto & include: qAsConst(additionalIncludes))
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            ctx.m_out << "#include " << include << ln;
        }
        else {
            ctx.m_out << "#include \"" << include << "\"" << ln;
        }
    }

    if (!additionalIncludes.isEmpty()) {
        ctx.m_out << ln;
    }

    for (const auto & fwd: qAsConst(forwardDeclarationsOutsideNamespace)) {
        ctx.m_out << fwd << ln;
    }

    if (!forwardDeclarationsOutsideNamespace.isEmpty()) {
        ctx.m_out << ln;
    }

    writeNamespaceBegin(ctx);
}

void Generator::writeHeaderBody(
    OutputFileContext & ctx, const QString & headerFileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind, const int depth)
{
    ctx.m_out << disclaimer << ln;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include <qevercloud/" << headerFileName << ">" << ln;
    }
    else {
        ctx.m_out << "#include \"" << headerFileName << "\"" << ln;
    }

    if (headerKind == HeaderKind::Test) {
        ctx.m_out << "#include \"";

        for (int i = 0; i < depth; ++i)
        {
            ctx.m_out << "../";
        }

        ctx.m_out << "../src/Impl.h\"" << ln;
    }
    else {
        ctx.m_out << "#include \"";

        for (int i = 0; i < depth; ++i)
        {
            ctx.m_out << "../";
        }

        ctx.m_out << "Impl.h\"" << ln;
    }

    for(const auto & include: additionalIncludes)
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            ctx.m_out << "#include " << include << ln;
        }
        else {
            ctx.m_out << "#include \"" << include << "\"" << ln;
        }
    }

    ctx.m_out << ln;
    writeNamespaceBegin(ctx);
}

void Generator::writeHeaderFooter(
    QTextStream & out, const QString & fileName,
    const QStringList & extraLinesInsideNamespace,
    const QStringList & extraLinesOutsideNamespace,
    const QString & section)
{
    for(const auto & line: extraLinesInsideNamespace) {
        out << line << ln;
    }

    if (!extraLinesInsideNamespace.empty()) {
        out << ln;
    }

    writeNamespaceEnd(out);

    if (!extraLinesOutsideNamespace.empty()) {
        out << ln;
    }

    for(const auto & line: extraLinesOutsideNamespace) {
        out << line << ln;
    }

    const QString guard = getIncludeGuard(fileName, section);

    out << ln;
    out << "#endif // " << guard << ln;
}

QString Generator::typeToStr(
    std::shared_ptr<Parser::Type> type, const QString & identifier,
    const MethodType methodType) const
{
    const auto primitiveType =
        std::dynamic_pointer_cast<Parser::PrimitiveType>(type);

    const auto stringType =
        std::dynamic_pointer_cast<Parser::StringType>(type);

    const auto byteArrayType =
        std::dynamic_pointer_cast<Parser::ByteArrayType>(type);

    const auto variantType =
        std::dynamic_pointer_cast<Parser::VariantType>(type);

    const auto voidType = std::dynamic_pointer_cast<Parser::VoidType>(type);

    const auto identifierType =
        std::dynamic_pointer_cast<Parser::IdentifierType>(type);

    const auto mapType = std::dynamic_pointer_cast<Parser::MapType>(type);
    const auto setType = std::dynamic_pointer_cast<Parser::SetType>(type);
    const auto listType = std::dynamic_pointer_cast<Parser::ListType>(type);
    const auto hashType = std::dynamic_pointer_cast<Parser::HashType>(type);

    QString result;

    QString typeName;
    if (methodType == MethodType::FuncParamType) {
        typeName = typeToStr(type, identifier, MethodType::TypeName);
    }

    if (primitiveType)
    {
        switch (primitiveType->m_type)
        {
        case Parser::PrimitiveType::Type::Bool:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("bool");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeBool(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readBool(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_BOOL");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default: result = QLatin1String("");
                }
            }
            break;
        case Parser::PrimitiveType::Type::Double:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("double");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeDouble(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readDouble(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_DOUBLE");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default:
                    result = QLatin1String("");
                }
            }
            break;
        case Parser::PrimitiveType::Type::Byte:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("quint8");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeByte(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readByte(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_BYTE");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default:
                    result = QLatin1String("");
                }
            }
            break;
        case Parser::PrimitiveType::Type::Int16:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("qint16");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeI16(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readI16(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_I16");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default:
                    result = QLatin1String("");
                }
            }
            break;
        case Parser::PrimitiveType::Type::Int32:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("qint32");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeI32(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readI32(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_I32");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default:
                    result = QLatin1String("");
                }
            }
            break;
        case Parser::PrimitiveType::Type::Int64:
            {
                switch(methodType)
                {
                case MethodType::TypeName:
                case MethodType::ReadTypeName:
                    result = QStringLiteral("qint64");
                    break;
                case MethodType::WriteMethod:
                    result = QStringLiteral("writer.writeI64(");
                    break;
                case MethodType::ReadMethod:
                    result = QStringLiteral("reader.readI64(");
                    break;
                case MethodType::ThriftFieldType:
                    result = QStringLiteral("ThriftFieldType::T_I64");
                    break;
                case MethodType::FuncParamType:
                    result = typeName;
                    break;
                default:
                    result = QLatin1String("");
                }
            }
            break;
        default:
            throw std::runtime_error{
                "Unexpected primitive type: " +
                QString::number(static_cast<qint64>(primitiveType->m_type)).toStdString()};
        }
    }
    else if (stringType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = QStringLiteral("QString");
            break;
        case MethodType::WriteMethod:
            result = QStringLiteral("writer.writeString(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("reader.readString(");
            break;
        case MethodType::ThriftFieldType:
            result = QStringLiteral("ThriftFieldType::T_STRING");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (byteArrayType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = QStringLiteral("QByteArray");
            break;
        case MethodType::WriteMethod:
            result = QStringLiteral("writer.writeBinary(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("reader.readBinary(");
            break;
        case MethodType::ThriftFieldType:
            result = QStringLiteral("ThriftFieldType::T_STRING");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (variantType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = QStringLiteral("QVariant");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (voidType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = QStringLiteral("void");
            break;
        default: result = QLatin1String("");
        }
    }
    else if (identifierType)
    {
        QString identifierTypeName = clearInclude(identifierType->m_identifier);
        if (methodType == MethodType::FuncParamType)
        {
            if (m_allEnums.contains(identifierTypeName))
            {
                result = typeName;
            }
            else
            {
                QString underlyingTypeName = aliasedTypeName(identifierTypeName);
                if (underlyingTypeName != identifierTypeName) {
                    result = typeName;
                }
                else {
                    QTextStream strm(&result);
                    strm << "const " << typeName << " &";
                }
            }
        }
        else if (methodType == MethodType::TypeName)
        {
            result = identifierTypeName;
        }
        else if (methodType == MethodType::ReadTypeName)
        {
            result = (identifierTypeName == QStringLiteral("Timestamp")
                      ? QStringLiteral("qint64")
                      : identifierTypeName);
        }
        else
        {
            if (const auto primitiveType = aliasedPrimitiveType(identifierTypeName))
            {
                const auto p = std::make_shared<Parser::PrimitiveType>(*primitiveType);
                result = typeToStr(p, identifier, methodType);
            }
            else if (m_stringTypeAliases.contains(identifierTypeName))
            {
                result = typeToStr(
                    std::make_shared<Parser::StringType>(), identifier,
                    methodType);
            }
            else if (m_byteArrayTypeAliases.contains(identifierTypeName))
            {
                result = typeToStr(
                    std::make_shared<Parser::ByteArrayType>(), identifier,
                    methodType);
            }
            else
            {
                if (m_allStructs.contains(identifierTypeName) ||
                    m_allExceptions.contains(identifierTypeName))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        {
                            QTextStream strm(&result);
                            strm << "write" << identifierTypeName << "(writer, ";
                        }
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "read" << identifierTypeName << "(reader, ";
                        }
                        break;
                    case MethodType::ThriftFieldType:
                        result = QStringLiteral("ThriftFieldType::T_STRUCT");
                        break;
                    default:
                        result = QLatin1String("");
                    }
                }
                else if (m_allEnums.contains(identifierTypeName))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        result = QStringLiteral("writer.writeI32(static_cast<qint32>(");
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "readEnum" << identifierTypeName << "(reader, ";
                        }
                        break;
                    case MethodType::ThriftFieldType:
                        result = QStringLiteral("ThriftFieldType::T_I32");
                        break;
                    default:
                        result = QLatin1String("");
                    }
                }
            }
        }
    }
    else if (mapType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            {
                QTextStream strm(&result);
                strm << "QMap<" << typeToStr(mapType->m_keyType, identifier)
                    << ", " << typeToStr(mapType->m_valueType, identifier)
                    << ">";
            }
            break;
        case MethodType::WriteMethod:
            result = QStringLiteral("writer.writeMapBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("reader.readMapBegin(");
            break;
        case MethodType::ThriftFieldType:
            result = QStringLiteral("ThriftFieldType::T_MAP");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (setType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            {
                QTextStream strm(&result);
                strm << "QSet<" << typeToStr(setType->m_valueType, identifier)
                    << ">";
            }
            break;
        case MethodType::WriteMethod:
            result = QStringLiteral("writer.writeSetBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("reader.readSetBegin(");
            break;
        case MethodType::ThriftFieldType:
            result = QStringLiteral("ThriftFieldType::T_SET");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (listType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            {
                // list<string> => QStringList
                QString valueType = typeToStr(listType->m_valueType, identifier);
                if (valueType == QStringLiteral("QString")) {
                    result = QStringLiteral("QStringList");
                }
                else {
                    QTextStream strm(&result);
                    strm << "QList<" << valueType << ">";
                }

                break;
            }
        case MethodType::WriteMethod:
            result = QStringLiteral("writer.writeListBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("reader.readListBegin(");
            break;
        case MethodType::ThriftFieldType:
            result = QStringLiteral("ThriftFieldType::T_LIST");
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }
    else if (hashType)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            {
                QTextStream strm(&result);
                strm << "QHash<" << typeToStr(hashType->m_keyType, identifier)
                    << ", " << typeToStr(hashType->m_valueType, identifier)
                    << ">";
            }
            break;
        case MethodType::FuncParamType:
            result = typeName;
            break;
        default:
            result = QLatin1String("");
        }
    }

    if (result.isEmpty() &&
        (methodType == MethodType::TypeName ||
         methodType == MethodType::ReadTypeName ||
         methodType == MethodType::FuncParamType ||
         methodType == MethodType::ThriftFieldType))
    {
        throw std::runtime_error(
            QString::fromUtf8("Error! unrecognized type (%1 of type %2, method type %3)")
            .arg(identifier, typeName, QString::number(static_cast<int>(methodType))).toStdString());
    }

    return result;
}

QString Generator::valueToStr(
    std::shared_ptr<Parser::ConstValue> value,
    std::shared_ptr<Parser::Type> type, const QString & identifier,
    const QString & offset)
{
    if (!value) {
        return QString();
    }

    auto mapType = std::dynamic_pointer_cast<Parser::MapType>(type);
    auto setType = std::dynamic_pointer_cast<Parser::SetType>(type);
    auto listType = std::dynamic_pointer_cast<Parser::ListType>(type);

    auto stringValue = std::dynamic_pointer_cast<Parser::StringValue>(value);
    auto literalValue = std::dynamic_pointer_cast<Parser::LiteralValue>(value);
    auto listValue = std::dynamic_pointer_cast<Parser::ListValue>(value);
    auto mapValue = std::dynamic_pointer_cast<Parser::MapValue>(value);

    QString result;
    if (stringValue)
    {
        QTextStream strm(&result);
        strm << "QStringLiteral(" << stringValue->m_value << ")";
    }
    else if (literalValue)
    {
        result = literalValue->m_value;
    }
    else if (listValue)
    {
        if (!setType && !listType) {
            throw std::runtime_error(QString::fromUtf8(
                "List initializer for an unsupported type for (%1)")
                .arg(identifier).toStdString());
        }

        result = typeToStr(type, identifier) + QStringLiteral("()");
        QString nextOffset = offset + QStringLiteral("    ");
        QTextStream strm(&result, QIODevice::Append);
        strm << ln;
        for(const auto & v: qAsConst(listValue->m_values)) {
            strm << offset << "<< "
                << valueToStr(v, std::shared_ptr<Parser::Type>(nullptr),
                              identifier, nextOffset)
                << ln;
        }
    }
    else if (mapValue) {
        throw std::runtime_error(
            QString::fromUtf8("map constants are not implemented (%1)")
            .arg(identifier).toStdString());
    }

    if (result.isEmpty()) {
        throw std::runtime_error(
            QString::fromUtf8("Error! unrecognized constant value (%1)")
            .arg(identifier).toStdString());
    }

    return result;
}

void Generator::generateConstantsHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Constants.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    writeHeaderHeader(
        ctx, fileName, QStringList() << QStringLiteral("<QtGlobal>"));

    ctx.m_out << blockSeparator << ln << ln;

    const auto & constants = parser.constants();
    for(const auto & c: constants)
    {
        if (c.m_fileName != fileName) {
            ctx.m_out << "// " << c.m_fileName << ln;
        }

        if (!c.m_docComment.isEmpty()) {
            ctx.m_out << c.m_docComment << ln;
        }

        ctx.m_out << "QEVERCLOUD_EXPORT extern const "
            << typeToStr(c.m_type, c.m_name)
            << " " << c.m_name << ";" << ln << ln;
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateConstantsCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Constants.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    writeHeaderBody(ctx, QStringLiteral("Constants.h"), {});

    ctx.m_out << blockSeparator << ln << ln;

    const auto & constants = parser.constants();
    for(const auto & c: constants)
    {
        if (c.m_fileName != fileName) {
            ctx.m_out << "// " << c.m_fileName << ln << ln;
        }

        if (!c.m_value) {
            throw std::runtime_error(
                QString::fromUtf8("Constant without a value: %1")
                .arg(c.m_name).toStdString());
        }

        ctx.m_out << "const " << typeToStr(c.m_type, c.m_name) << " "
            << c.m_name << " = "
            << valueToStr(c.m_value, c.m_type, c.m_name, QStringLiteral("    "))
            << ";" << ln;
    }

    ctx.m_out << ln;
    writeNamespaceEnd(ctx.m_out);
}

QString Generator::getIncludeGuard(
    const QString & fileName, const QString & section) const
{
    QString guard;
    QTextStream strm(&guard);

    strm << "QEVERCLOUD_GENERATED_";
    if (!section.isEmpty()) {
        auto s = section;
        s.replace(QChar::fromLatin1('/'), QChar::fromLatin1('_'));
        strm << s.toUpper();
        strm << QStringLiteral("_");
    }
    strm << fileName.split(QChar::fromLatin1('.'))[0].toUpper();
    strm.flush();
    return guard;
}

QString Generator::getIdentifier(const std::shared_ptr<Parser::Type> & type)
{
    auto it = std::dynamic_pointer_cast<Parser::IdentifierType>(type);
    return (it
            ? clearInclude(it->m_identifier)
            : QString());
}

void Generator::writeThriftWriteFields(
    QTextStream & out, const QList<Parser::Field> & fields,
    const QString & indentPrefix, const QString & fieldPrefix)
{
    for(const auto & field: fields)
    {
        if (field.m_affiliation != Parser::Field::Affiliation::Evernote) {
            continue;
        }

        QString indentStr = QLatin1String("");

        const bool isOptional =
            (field.m_required == Parser::Field::RequiredFlag::Optional);

        const QString fieldSuffix =
            (fieldPrefix.isEmpty() ? QLatin1String("") : QStringLiteral("()"));

        if (isOptional) {
            indentStr = QStringLiteral("    ");
            out << "    if (" << fieldPrefix
                << field.m_name << fieldSuffix << ") {" << ln;
        }

        out << indentStr << "    writer.writeFieldBegin(" << ln
            << indentStr << "        QStringLiteral(\""
            << field.m_name << "\")," << ln
            << indentStr << "        " << typeToStr(
                field.m_type, indentPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << "," << ln
            << indentStr << "        " << field.m_id << ");" << ln << ln;

        QString writeMethod = typeToStr(
            field.m_type, indentPrefix + QStringLiteral(",") + field.m_name,
            MethodType::WriteMethod);

        if (writeMethod.contains(QStringLiteral("writeListBegin")))
        {
            auto valueType = std::dynamic_pointer_cast<Parser::ListType>(
                field.m_type)->m_valueType;

            out << indentStr << "    writer.writeListBegin("
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "length());" << ln;

            out << indentStr
                << "    for(const auto & value: qAsConst("
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indentStr << "        " << writeMethod << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indentStr << "    }" << ln;
            out << indentStr << "    writer.writeListEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            auto valueType = std::dynamic_pointer_cast<Parser::SetType>(
                field.m_type)->m_valueType;

            out << indentStr << "    writer.writeSetBegin("
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "count());" << ln;

            out << indentStr << "    for(const auto & value: qAsConst("
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indentStr << "        " << writeMethod
                << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indentStr << "    }" << ln;
            out << indentStr << "    writer.writeSetEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            auto keyType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_keyType;

            auto valueType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_valueType;

            out << indentStr << "    writer.writeMapBegin("
                << typeToStr(
                    keyType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", "
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "size());" << ln;

            out << indentStr << "    for(const auto & it: "
                << "toRange(" << (isOptional ? "*" : "") << fieldPrefix
                << field.m_name << fieldSuffix << ")) {" << ln;

            QString keyWriteMethod = typeToStr(
                keyType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            QString valueWriteMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indentStr << "        " << keyWriteMethod
                << "it.key()"
                << (keyWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indentStr << "        " << valueWriteMethod << "it.value()"
                << (valueWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indentStr << "    }" << ln;
            out << indentStr << "    writer.writeMapEnd();" << ln << ln;
        }
        else
        {
            out << indentStr << "    " << writeMethod
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;
        }

        out << indentStr << "    writer.writeFieldEnd();" << ln;
        if (isOptional) {
            out << "    }" << ln;
        }
        out << ln;
    }
}

void Generator::writeThriftReadField(
    QTextStream & out, const Parser::Field & field, const QString & indentPrefix,
    const QString & fieldParent)
{
    constexpr const char * longIndent = "                ";

    out << longIndent
        << typeToStr(
            field.m_type, indentPrefix + field.m_name, MethodType::ReadTypeName)
        << " v;" << ln;

    QString readMethod = typeToStr(
        field.m_type, indentPrefix + field.m_name, MethodType::ReadMethod);
    if (readMethod.contains(QStringLiteral("readListBegin")))
    {
        auto valueType =
            std::dynamic_pointer_cast<Parser::ListType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType,  indentPrefix + field.m_name,
            MethodType::ThriftFieldType);

        out << longIndent << "qint32 size;" << ln;
        out << longIndent << "ThriftFieldType elemType;" << ln;
        out << longIndent << "reader.readListBegin(elemType, size);" << ln;
        out << longIndent << "v.reserve(size);" << ln;
        out << longIndent << "if (elemType != " << valueThriftType
            << ") {" << ln << longIndent << "    throw ThriftException("
            << ln << longIndent << "        ThriftException::Type::"
            << "INVALID_DATA," << ln << longIndent
            << "        QStringLiteral(\"Incorrect list type ("
            << indentPrefix + field.m_name << ")\"));" << ln
            << longIndent << "}" << ln;
        out << longIndent << "for(qint32 i = 0; i < size; i++) {"
            << ln;
        out << longIndent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << ln;
        out << longIndent << "    " << valueReadMethod << "elem);" << ln;
        out << longIndent << "    v.append(elem);" << ln;
        out << longIndent << "}" << ln;
        out << longIndent << "reader.readListEnd();" << ln;
    }
    else if (readMethod.contains(QStringLiteral("readSetBegin")))
    {
        auto valueType =
            std::dynamic_pointer_cast<Parser::SetType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ThriftFieldType);

        out << longIndent << "qint32 size;" << ln;
        out << longIndent << "ThriftFieldType elemType;" << ln;
        out << longIndent << "reader.readSetBegin(elemType, size);" << ln;
        out << longIndent << "v.reserve(size);" << ln;
        out << longIndent << "if (elemType != " << valueThriftType
            << ") {" << ln
            << longIndent << "    throw ThriftException(" << ln
            << longIndent << "        ThriftException::Type::INVALID_DATA,"
            << ln
            << longIndent << "        QStringLiteral(\"Incorrect set type ("
            << indentPrefix + field.m_name << ")\"));" << ln
            << longIndent << "}" << ln;
        out << longIndent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << longIndent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << ln;
        out << longIndent << "    " << valueReadMethod << "elem);" << ln;
        out << longIndent << "    v.insert(elem);" << ln;
        out << longIndent << "}" << ln;
        out << longIndent << "reader.readSetEnd();" << ln;
    }
    else if (readMethod.contains(QStringLiteral("readMapBegin")))
    {
        auto keyType =
            std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_keyType;

        QString keyReadMethod = typeToStr(
            keyType, indentPrefix + field.m_name, MethodType::ReadMethod);

        QString keyThriftType = typeToStr(
            keyType, indentPrefix + field.m_name, MethodType::ThriftFieldType);

        auto valueType =
            std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ThriftFieldType);

        out << longIndent << "qint32 size;" << ln;
        out << longIndent << "ThriftFieldType keyType;" << ln;
        out << longIndent << "ThriftFieldType elemType;" << ln;
        out << longIndent << "reader.readMapBegin(keyType, elemType, size);"
            << ln;
        out << longIndent << "if (keyType != " << keyThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map key type ("
            << indentPrefix << field.m_name << ")\"));" << ln;
        out << longIndent << "if (elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map value type ("
            << indentPrefix + field.m_name << ")\"));" << ln;
        out << longIndent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << longIndent << "    "
            << typeToStr(
                keyType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " key;" << ln;
        out << longIndent << "    " << keyReadMethod << "key);" << ln;
        out << longIndent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " value;" << ln;
        out << longIndent << "    " << valueReadMethod << "value);" << ln;
        out << longIndent << "    v[key] = value;" << ln;
        out << longIndent << "}" << ln;
        out << longIndent << "reader.readMapEnd();" << ln;
    }
    else
    {
        out << longIndent << readMethod << "v);" << ln;
    }

    if (fieldParent.isEmpty()) {
        out << longIndent << field.m_name << " = ";
    }
    else {
        out << longIndent << fieldParent << fieldSetterName(field) << "(";
    }

    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        out << "std::make_optional(v)";
    }
    else {
        out << "v";
    }

    if (fieldParent.isEmpty()) {
        out << ";" << ln;
    }
    else {
        out << ");" << ln;
    }
}

void Generator::sortIncludes(QStringList & includes) const
{
    std::sort(includes.begin(), includes.end(),
          [&] (const QString & lhs, const QString & rhs) {
              if (!lhs.startsWith(QChar::fromLatin1('<')))
              {
                  if (!rhs.startsWith(QChar::fromLatin1('<'))) {
                      return lhs < rhs;
                  }

                  return true;
              }

              if (lhs.startsWith(QChar::fromLatin1('<')) &&
                  lhs.endsWith(QStringLiteral(".h>")))
              {
                  if (!rhs.startsWith(QChar::fromLatin1('<'))) {
                      return false;
                  }

                  if (rhs.endsWith(QStringLiteral(".h>"))) {
                      return lhs < rhs;
                  }

                  return true;
              }

              if (lhs.startsWith(QStringLiteral("<Q")) &&
                  !lhs.endsWith(QStringLiteral(".h>")))
              {
                  if (!rhs.startsWith(QChar::fromLatin1('<'))) {
                      return false;
                  }

                  if (rhs.endsWith(QStringLiteral(".h>"))) {
                      return false;
                  }

                  if (rhs.startsWith(QStringLiteral("<Q")) &&
                      !rhs.endsWith(QStringLiteral(".h>")))
                  {
                      return lhs < rhs;
                  }

                  return true;
              }

              if (!rhs.startsWith(QChar::fromLatin1('<'))) {
                  return false;
              }

              if (rhs.endsWith(QStringLiteral(".h>"))) {
                  return false;
              }

              if (rhs.startsWith(QStringLiteral("<Q")) &&
                  !rhs.endsWith(QStringLiteral(".h>")))
              {
                  return false;
              }

              return lhs < rhs;
          });
}

void Generator::generateErrorsHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("EDAMErrorCode.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    auto additionalIncludes = QStringList()
        << QStringLiteral("<QDebug>") << QStringLiteral("<QMetaType>")
        << QStringLiteral("<QTextStream>");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    ctx.m_out << blockSeparator << ln << ln
        << "#if QT_VERSION >= QT_VERSION_CHECK(5, 8, 0)" << ln
        << "#if QEVERCLOUD_USES_Q_NAMESPACE" << ln
        << "Q_NAMESPACE" << ln
        << "#endif" << ln
        << "#endif" << ln << ln
        << blockSeparator << ln << ln;

    const auto & enumerations = parser.enumerations();
    int enumerationsCount = enumerations.size();
    int i = 0;
    for(const auto & e: enumerations)
    {
        writeEnumeration(ctx, e);
        ctx.m_out << blockSeparator << ln << ln;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QTextStream");
        ctx.m_out << blockSeparator << ln << ln;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QDebug");
        if (i != (enumerationsCount - 1)) {
            ctx.m_out << blockSeparator << ln << ln;
        }

        ++i;
    }

    QStringList extraLinesOutsideNamespace;
    extraLinesOutsideNamespace.reserve(enumerations.size() + 2);
    extraLinesOutsideNamespace << QStringLiteral(
        "// NOTE: explicit metatype declarations are not needed if "
        "QEVERCLOUD_USES_Q_NAMESPACE is defined");
    extraLinesOutsideNamespace << QStringLiteral(
        "// NOTE: but since it is not necessarily defined, explicit metatype "
        "declarations are here");
    for(const auto & e: enumerations)
    {
        QString line;
        QTextStream lineOut(&line);

        lineOut << "Q_DECLARE_METATYPE(qevercloud::" << e.m_name << ")";
        lineOut.flush();

        extraLinesOutsideNamespace << line;
    }

    writeHeaderFooter(ctx.m_out, fileName, {}, extraLinesOutsideNamespace);
}

void Generator::generateErrorsCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("EDAMErrorCode.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    writeHeaderBody(ctx, QStringLiteral("EDAMErrorCode.h"));

    ctx.m_out << blockSeparator << ln << ln;

    const auto & enumerations = parser.enumerations();
    int enumerationsCount = enumerations.size();
    int i = 0;
    for(const auto & e: enumerations)
    {
        writeEnumerationPrintDefinition(ctx.m_out, e, "QTextStream");
        ctx.m_out << blockSeparator << ln << ln;

        writeEnumerationPrintDefinition(ctx.m_out, e, "QDebug");
        if (i != (enumerationsCount - 1)) {
            ctx.m_out << blockSeparator << ln << ln;
        }

        ++i;
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTypesIOHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types_io.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("types"));

    auto additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/exceptions/All.h>")
        << QStringLiteral("<qevercloud/types/All.h>")
        << QStringLiteral("../Impl.h");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << ln << ln;

    QList<const QList<Parser::Structure>*> lists;
    lists.reserve(2);
    lists << &parser.structures();
    lists << &parser.exceptions();

    for(const auto & pList: qAsConst(lists))
    {
        for(const auto & s: *pList) {
            ctx.m_out << "void write" << s.m_name
                << "(ThriftBinaryBufferWriter & writer, const "
                << s.m_name << " & s);" << ln;
            ctx.m_out << "void read" << s.m_name
                << "(ThriftBinaryBufferReader & reader, "
                << s.m_name << " & s);" << ln;
        }
    }
    ctx.m_out << ln;

    const auto & enumerations = parser.enumerations();
    for(const auto & e: enumerations) {
        ctx.m_out << "void readEnum" << e.m_name
            << "(ThriftBinaryBufferReader & reader, "
            << e.m_name << " & e);" << ln;
    }
    ctx.m_out << ln << "/** @endcond */" << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypesIOCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types_io.cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("types"));

    auto additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/exceptions/All.h>")
        << QStringLiteral("<qevercloud/types/All.h>")
        << QStringLiteral("<QUuid>") << QStringLiteral("<QDebug>")
        << QStringLiteral("<optional>");

    sortIncludes(additionalIncludes);

    writeHeaderBody(
        ctx, QStringLiteral("Types_io.h"), additionalIncludes,
        HeaderKind::Private, 1);

    ctx.m_out << blockSeparator << ln << ln;
    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << ln << ln;

    const auto & enumerations = parser.enumerations();
    for(const auto & e: enumerations)
    {
        ctx.m_out <<  "void readEnum" << e.m_name
            << "(" << ln << "    ThriftBinaryBufferReader & reader," << ln
            << "    " << e.m_name << " & e)" << ln << "{" << ln;

        ctx.m_out << "    qint32 i;" << ln;
        ctx.m_out << "    reader.readI32(i);" << ln;
        ctx.m_out << "    switch(i) {" << ln;

        for(const auto & v : e.m_values) {
            QString value = e.m_name + QStringLiteral("::") + v.first;
            ctx.m_out << "    case static_cast<int>("
                << value << "): e = " << value
                << "; break;" << ln;
        }

        ctx.m_out << "    default: throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect value for enum "
            << e.m_name << "\"));" << ln;
        ctx.m_out << "    }" << ln;
        ctx.m_out << "}" << ln << ln;
    }

    QSet<QString> exceptions;
    for(const auto & e: parser.exceptions()) {
        exceptions.insert(e.m_name);
    }

    auto structsAndExceptions = parser.structures();
    structsAndExceptions << parser.exceptions();

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        ctx.m_out << "void write" << s.m_name
            << "(" << ln << "    ThriftBinaryBufferWriter & writer," << ln
            << "    const "
            << s.m_name << " & s)" << ln << "{" << ln;

        ctx.m_out << "    writer.writeStructBegin(QStringLiteral(\""
            << s.m_name  << "\"));" << ln;

        writeThriftWriteFields(
            ctx.m_out, s.m_fields, s.m_name, QStringLiteral("s."));

        ctx.m_out << "    writer.writeFieldStop();" << ln;
        ctx.m_out << "    writer.writeStructEnd();" << ln;
        ctx.m_out << "}" << ln << ln;

        ctx.m_out << "void read" << s.m_name
            << "(" << ln << "    ThriftBinaryBufferReader & reader," << ln
            << "    " << s.m_name << " & s)" << ln << "{" << ln;
        ctx.m_out << "    QString fname;" << ln;
        ctx.m_out << "    ThriftFieldType fieldType;" << ln;
        ctx.m_out << "    qint16 fieldId;" << ln;

        for(const auto & field : s.m_fields)
        {
            if (field.m_affiliation != Parser::Field::Affiliation::Evernote) {
                continue;
            }

            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    bool " << field.m_name
                    << "_isset = false;" << ln;
            }
        }

        ctx.m_out << "    reader.readStructBegin(fname);" << ln;
        ctx.m_out << "    while(true)" << ln
            << "    {" << ln;
        ctx.m_out << "        reader.readFieldBegin(fname, fieldType, fieldId);"
            << ln;
        ctx.m_out << "        if (fieldType == "
            << "ThriftFieldType::T_STOP) break;" << ln;

        for(const auto & field : s.m_fields)
        {
            if (field.m_affiliation != Parser::Field::Affiliation::Evernote) {
                continue;
            }

            bool isOptional =
                (field.m_required == Parser::Field::RequiredFlag::Optional);
            ctx.m_out << "        if (fieldId == " << field.m_id
                << ") {" << ln;
            ctx.m_out << "            if (fieldType == "
                << typeToStr(
                    field.m_type, s.m_name + QStringLiteral(".") + field.m_name,
                    MethodType::ThriftFieldType)
                << ") {" << ln;

            if (!isOptional) {
                ctx.m_out << "                " << field.m_name
                    << "_isset = true;" << ln;
            }

            writeThriftReadField(
                ctx.m_out, field, s.m_name + QStringLiteral("."),
                QStringLiteral("s."));

            ctx.m_out << "            }" << ln
                << "            else {" << ln;
            ctx.m_out << "                reader.skip(fieldType);" << ln;
            ctx.m_out << "            }" << ln;
            ctx.m_out << "        }" << ln
                << "        else" << ln;
        }

        ctx.m_out << "        {" << ln;
        ctx.m_out << "            reader.skip(fieldType);" << ln;
        ctx.m_out << "        }" << ln;
        ctx.m_out << "        reader.readFieldEnd();" << ln;
        ctx.m_out << "    }" << ln;
        ctx.m_out << "    reader.readStructEnd();" << ln;

        for(const auto & field : s.m_fields)
        {
            if (field.m_affiliation != Parser::Field::Affiliation::Evernote) {
                continue;
            }

            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    if (!" << field.m_name
                    << "_isset) throw ThriftException("
                    << "ThriftException::Type::INVALID_DATA, "
                    << "QStringLiteral(\""
                    << s.m_name << "." << field.m_name
                    << " has no value\"));"
                    << ln;
            }
        }

        ctx.m_out << "}" << ln << ln;
    }

    ctx.m_out << "/** @endcond */" << ln << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateAllExceptionsHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("All.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("exceptions"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_EXCEPTIONS_ALL_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_EXCEPTIONS_ALL_H" << ln << ln;

    const auto & exceptions = parser.exceptions();
    QStringList exceptionIncludes;
    exceptionIncludes.reserve(exceptions.size() + 6);
    for (const auto & s: exceptions) {
        exceptionIncludes << s.m_name;
    }

    exceptionIncludes << QStringLiteral("EDAMSystemExceptionAuthExpired")
        << QStringLiteral("EDAMSystemExceptionRateLimitReached")
        << QStringLiteral("EverCloudException")
        << QStringLiteral("EvernoteException")
        << QStringLiteral("NetworkException")
        << QStringLiteral("ThriftException");

    std::sort(exceptionIncludes.begin(), exceptionIncludes.end());

    for (const auto & include: qAsConst(exceptionIncludes)) {
        ctx.m_out << "#include \"" << include << ".h\"" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_EXCEPTIONS_ALL_H" << ln;
}

void Generator::generateExceptionsFwdHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("exceptions"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_EXCEPTIONS_FWD_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_EXCEPTIONS_FWD_H" << ln << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln;

    const auto & exceptions = parser.exceptions();
    QStringList exceptionClasses;
    exceptionClasses.reserve(exceptions.size() + 6);
    for (const auto & s: exceptions) {
        exceptionClasses << s.m_name;
    }

    exceptionClasses << QStringLiteral("EDAMSystemExceptionAuthExpired")
        << QStringLiteral("EDAMSystemExceptionRateLimitReached")
        << QStringLiteral("EverCloudException")
        << QStringLiteral("EvernoteException")
        << QStringLiteral("NetworkException")
        << QStringLiteral("ThriftException");

    std::sort(exceptionClasses.begin(), exceptionClasses.end());

    for (const auto & exceptionClass: qAsConst(exceptionClasses)) {
        ctx.m_out << "class " << exceptionClass << ";" << ln;
    }

    ctx.m_out << ln << "} // namespace qevercloud" << ln << ln;

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_EXCEPTIONS_FWD_H" << ln;
}

void Generator::generateAllTypesHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("All.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, QStringLiteral("types"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_TYPES_ALL_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_TYPES_ALL_H" << ln << ln;

    const auto & structures = parser.structures();
    QStringList typesIncludes;
    typesIncludes.reserve(structures.size());
    for (const auto & s: structures) {
        typesIncludes << s.m_name;
    }

    typesIncludes << QStringLiteral("TypeAliases");

    std::sort(typesIncludes.begin(), typesIncludes.end());

    for (const auto & include: qAsConst(typesIncludes)) {
        ctx.m_out << "#include \"" << include << ".h\"" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_TYPES_ALL_H" << ln;
}

void Generator::generateTypesFwdHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, QStringLiteral("types"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_TYPES_FWD_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_TYPES_FWD_H" << ln << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln;

    const auto & structures = parser.structures();
    QStringList types;
    types.reserve(structures.size());
    for (const auto & s: structures) {
        types<< s.m_name;
    }

    std::sort(types.begin(), types.end());

    for (const auto & type: qAsConst(types)) {
        ctx.m_out << "class " << type << ";" << ln;
    }

    ctx.m_out << ln << "} // namespace qevercloud" << ln << ln;

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_TYPES_FWD_H" << ln;
}

void Generator::generateTypeAliasesHeader(
    const Parser::TypeAliases & typeAliases, const QString & outPath)
{
    const QString fileName = QStringLiteral("TypeAliases.h");
    const QString fileSection = QStringLiteral("types");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, fileSection);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("<QString>") << QStringLiteral("<QtGlobal>");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Public);

    for(const auto & t: typeAliases)
    {
        if (!t.m_docComment.isEmpty()) {
            ctx.m_out << t.m_docComment << ln;
        }

        ctx.m_out << "using " << t.m_name << " = "
            << typeToStr(t.m_type, t.m_name) << ";" << ln << ln;
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypeHeader(
    const Parser::Structure & s, const QString & outPath,
    const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral(".h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, fileSection);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/utility/Printable.h>")
        << QStringLiteral("<qevercloud/EDAMErrorCode.h>")
        << QStringLiteral("<qevercloud/types/TypeAliases.h>")
        << QStringLiteral("<QSharedDataPointer>")
        << additionalIncludesForFields(s);

    const bool isExceptionsSection =
        (fileSection == QStringLiteral("exceptions"));

    if (isExceptionsSection) {
        additionalIncludes
            << QStringLiteral("<qevercloud/exceptions/EvernoteException.h>");
    }
    else {
        additionalIncludes
            << QStringLiteral("<qevercloud/exceptions/EverCloudException.h>");
    }

    const bool hasLocalDataField = [&]
    {
        if (isExceptionsSection) {
            return false;
        }

        const auto it = std::find_if(
            s.m_fields.constBegin(),
            s.m_fields.constEnd(),
            [](const Parser::Field & field)
            {
                return field.m_name == QStringLiteral("localData") &&
                    dynamic_cast<Parser::HashType*>(field.m_type.get());
            });
        return it != s.m_fields.constEnd();
    }();

    if (hasLocalDataField) {
        additionalIncludes << QStringLiteral("<QHash>")
            << QStringLiteral("<QVariant>");
    }

    const auto deps = dependentTypeNames(s);
    for (const auto & dep: qAsConst(deps)) {
        if (dep == QStringLiteral("QString")) {
            continue;
        }

        if (dep == QStringLiteral("QByteArray")) {
            additionalIncludes.append(QStringLiteral("<QByteArray>"));
            continue;
        }

        const bool isExceptionType = m_allExceptions.contains(dep);
        if (isExceptionType && !isExceptionsSection) {
            additionalIncludes.append(
                QStringLiteral("<qevercloud/exceptions/") + dep +
                QStringLiteral(".h>"));
            continue;
        }

        if (!isExceptionType && isExceptionsSection) {
            additionalIncludes.append(
                QStringLiteral("<qevercloud/types/") + dep +
                QStringLiteral(".h>"));
            continue;
        }

        additionalIncludes.append(dep + QStringLiteral(".h"));
    }

    sortIncludes(additionalIncludes);
    writeHeaderHeader(ctx, fileName, additionalIncludes);

    if (!s.m_docComment.isEmpty()) {
        ctx.m_out << s.m_docComment << ln;
    }

    ctx.m_out << "class QEVERCLOUD_EXPORT " << s.m_name << ": ";

    if (isExceptionsSection) {
        ctx.m_out << "public EvernoteException, ";
    }

    ctx.m_out << "public Printable" << ln
        << "{" << ln
        << indent << "Q_GADGET" << ln
        << "public:" << ln;

    ctx.m_out << indent << s.m_name << "();" << ln;

    ctx.m_out << indent << s.m_name << "(const "
        << s.m_name << " & other);" << ln;

    ctx.m_out << indent << s.m_name << "(" << s.m_name << " && other) noexcept;"
        << ln;

    ctx.m_out << indent << "~" << s.m_name << "() noexcept override;" << ln;

    ctx.m_out << ln;

    ctx.m_out << indent << s.m_name << " & operator=(const " << s.m_name
        << " & other);" << ln;

    ctx.m_out << indent << s.m_name << " & operator=(" << s.m_name
        << " && other) noexcept;" << ln << ln;

    for(const auto & f: qAsConst(s.m_fields))
    {
        if (const auto cit = s.m_fieldComments.constFind(f.m_name);
            cit != s.m_fieldComments.constEnd())
        {
            const auto lines = cit.value().split(QChar::fromLatin1('\n'));
            for(const auto & line: qAsConst(lines))
            {
                if (&line != &lines.front() && &line != &lines.back())
                {
                    const auto simplifiedLine = line.simplified();
                    if (simplifiedLine != QStringLiteral("<br/>"))
                    {
                        ctx.m_out << indent << " *";
                        if (!simplifiedLine.isEmpty()) {
                            ctx.m_out << " " << simplifiedLine;
                        }
                        ctx.m_out << ln;
                    }
                }
                else
                {
                    ctx.m_out << indent;
                    if (&line == &lines.back()) {
                        ctx.m_out << " ";
                    }
                    ctx.m_out << line << ln;
                }
            }
        }

        generateClassAccessoryMethodsForFieldDeclarations(f, ctx);
        ctx.m_out << ln;
    }

    ctx.m_out << indent
        << "void print(QTextStream & strm) const override;" << ln << ln;

    ctx.m_out << indent
        << "friend QEVERCLOUD_EXPORT QTextStream & operator<<(" << ln
        << indent << indent << "QTextStream & strm, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ");" << ln << ln;

    ctx.m_out << indent
        << "friend QEVERCLOUD_EXPORT QDebug & operator<<(" << ln
        << indent << indent << "QDebug & dbg, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ");" << ln << ln;

    ctx.m_out << indent
        << "friend QEVERCLOUD_EXPORT std::ostream & operator<<(" << ln
        << indent << indent << "std::ostream & strm, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ");" << ln;

    if (m_allExceptions.contains(s.m_name)) {
        ctx.m_out << ln;
        ctx.m_out << indent
            << "[[nodiscard]] const char * what() const noexcept override;"
            << ln;

        ctx.m_out << indent << "void raise() const override;" << ln;
        ctx.m_out << indent << "[[nodiscard]] " << s.m_name << " * clone() "
            << "const override;" << ln;
    }

    ctx.m_out << ln;

    writeTypeProperties(s, ctx);

    ctx.m_out << ln;
    ctx.m_out << "private:" << ln
        << indent << "class Impl;" << ln
        << indent << "QSharedDataPointer<Impl> d;" << ln
        << "};" << ln << ln;

    ctx.m_out << "[[nodiscard]] QEVERCLOUD_EXPORT bool operator==(const "
        << s.m_name << " & lhs, const " << s.m_name << " & rhs) noexcept;"
        << ln;

    ctx.m_out << "[[nodiscard]] QEVERCLOUD_EXPORT bool operator!=(const "
        << s.m_name << " & lhs, const " << s.m_name << " & rhs) noexcept;"
        << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypeCpp(
    const Parser::Structure & s, const QString & outPath,
    const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral(".cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation, fileSection);

    ctx.m_out << disclaimer << ln;

    ctx.m_out << "#include <qevercloud/" << fileSection << "/" << s.m_name
        << ".h>" << ln << ln;

    ctx.m_out << "#include \"impl/" << s.m_name << "Impl.h\"" << ln << ln;

    const bool isException = m_allExceptions.contains(s.m_name);
    if (isException) {
        ctx.m_out << "#include <memory>" << ln << ln;
    }

    writeNamespaceBegin(ctx);

    ctx.m_out << s.m_name << "::" << s.m_name << "() :" << ln;

    if (isException) {
        ctx.m_out << indent << "EvernoteException(QStringLiteral(\""
            << s.m_name << "\"))," << ln;
    }

    ctx.m_out << indent << "d(new " << s.m_name
        << "::Impl)" << ln;

    ctx.m_out << "{}" << ln << ln;

    ctx.m_out << s.m_name << "::" << s.m_name << "(const " << s.m_name
        << " & other) :" << ln
        << indent << "d(other.d)" << ln
        << "{}" << ln << ln;

    ctx.m_out << s.m_name << "::" << s.m_name << "(" << s.m_name
        << " && other) noexcept :" << ln
        << indent << "d(std::move(other.d))" << ln
        << "{}" << ln << ln;

    ctx.m_out << s.m_name << "::~" << s.m_name << "() noexcept {}" << ln << ln;

    ctx.m_out << s.m_name << " & " << s.m_name << "::operator=(const "
        << s.m_name << " & other)" << ln
        << "{" << ln
        << indent << "if (this != &other) {" << ln
        << indent << indent << "d = other.d;" << ln
        << indent << "}" << ln << ln
        << indent << "return *this;" << ln
        << "}" << ln << ln;

    ctx.m_out << s.m_name << " & " << s.m_name << "::operator=(" << s.m_name
        << " && other) noexcept" << ln
        << "{" << ln
        << indent << "if (this != &other) {" << ln
        << indent << indent << "d = std::move(other.d);" << ln
        << indent << "}" << ln << ln
        << indent << "return *this;" << ln
        << "}" << ln << ln;

    for(const auto & f: qAsConst(s.m_fields)) {
        generateClassAccessoryMethodsForFieldDefinitions(s, f, ctx);
    }

    ctx.m_out << "void " << s.m_name
        << "::print(QTextStream & strm) const" << ln
        << "{" << ln
        << indent << "d->print(strm);" << ln
        << "}" << ln << ln;

    if (isException)
    {
        generateExceptionClassWhatMethodDefinition(s, ctx);
        generateExceptionClassRaiseMethodDefinition(s, ctx);
        generateExceptionClassCloneMethodDefinition(s, ctx);
    }

    ctx.m_out << "QTextStream & operator<<(QTextStream & strm, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ")" << ln
        << "{" << ln
        << indent << "strm << static_cast<const Printable&>("
        << decapitalize(s.m_name) << ");" << ln
        << indent << "return strm;" << ln
        << "}" << ln << ln;

    ctx.m_out << "QDebug & operator<<(QDebug & dbg, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ")" << ln
        << "{" << ln
        << indent << "dbg << static_cast<const Printable&>("
        << decapitalize(s.m_name) << ");" << ln
        << indent << "return dbg;" << ln
        << "}" << ln << ln;

    ctx.m_out << "std::ostream & operator<<(std::ostream & strm, const "
        << s.m_name << " & " << decapitalize(s.m_name) << ")" << ln
        << "{" << ln
        << indent << "strm << static_cast<const Printable&>("
        << decapitalize(s.m_name) << ");" << ln
        << indent << "return strm;" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool operator==(const " << s.m_name
        << " & lhs, const " << s.m_name << " & rhs) noexcept" << ln
        << "{" << ln;

    ctx.m_out << indent << "if (&lhs == &rhs) {" << ln
        << indent << indent << "return true;" << ln
        << indent << "}" << ln << ln;

    ctx.m_out << indent << "return" << ln;

    for(const auto & f: qAsConst(s.m_fields))
    {
        ctx.m_out << indent << indent << "lhs." << f.m_name << "() == rhs."
            << f.m_name << "()";

        if (&f != &(s.m_fields.constLast())) {
            ctx.m_out << " &&" << ln;
        }
        else {
            ctx.m_out << ";" << ln;
        }
    }

    ctx.m_out << "}" << ln << ln;

    ctx.m_out << "bool operator!=(const " << s.m_name
        << " & lhs, const " << s.m_name << " & rhs) noexcept" << ln
        << "{" << ln
        << indent << "return !operator==(lhs, rhs);" << ln
        << "}" << ln << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTypeImplHeader(
    const Parser::Structure & s, const Parser::Enumerations & enumerations,
    const QString & outPath, const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral("Impl.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        fileSection + QStringLiteral("/impl"));

    const QString publicHeaderInclude =
        QStringLiteral("<qevercloud/") + fileSection +
        QStringLiteral("/") + s.m_name + QStringLiteral(".h>");

    QStringList additionalIncludes = QStringList()
        << publicHeaderInclude << QStringLiteral("<QSharedData>");

    if (structContainsLocalFields(s)) {
        additionalIncludes << QStringLiteral("<QHash>")
            << QStringLiteral("<QVariant>");
    }

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << "class Q_DECL_HIDDEN " << s.m_name << "::"
        << "Impl final:" << ln
        << indent << "public QSharedData," << ln
        << indent << "public Printable" << ln
        << "{" << ln
        << "public:" << ln;

    if (structContainsLocalIdField(s)) {
        ctx.m_out << indent << "Impl();" << ln;
    }
    else {
        ctx.m_out << indent << "Impl() = default;" << ln;
    }

    ctx.m_out << indent << "Impl(const " << s.m_name
        << "::Impl & other) = default;" << ln
        << indent << "Impl(" << s.m_name
        << "::Impl && other) noexcept = default;" << ln << ln;

    ctx.m_out << indent << s.m_name << "::Impl & operator=(const "
        << s.m_name << "::Impl & other) = delete;" << ln
        << indent << s.m_name << "::Impl & operator=(" << s.m_name
        << "::Impl && other) = delete;" << ln << ln;

    ctx.m_out << indent << "~Impl() noexcept override = default;" << ln;

    ctx.m_out << ln
        << indent << "void print(QTextStream & strm) const override;" << ln
        << ln;

    for(const auto & f: qAsConst(s.m_fields))
    {
        const auto typeName = typeToStr(f.m_type, {}, MethodType::TypeName);

        if (f.m_required == Parser::Field::RequiredFlag::Optional) {
            ctx.m_out << indent << "std::optional<" << typeName << "> "
                << "m_" << f.m_name << ";" << ln;
            continue;
        }

        ctx.m_out << indent << typeName << " " << "m_" << f.m_name;

        const auto a = aliasedPrimitiveType(typeName);
        const auto p = dynamic_cast<Parser::PrimitiveType*>(f.m_type.get());
        if (a || p)
        {
            switch(a ? *a : p->m_type)
            {
            case Parser::PrimitiveType::Type::Bool:
                ctx.m_out << " = false";
                break;
            case Parser::PrimitiveType::Type::Byte:
            case Parser::PrimitiveType::Type::Int16:
            case Parser::PrimitiveType::Type::Int32:
            case Parser::PrimitiveType::Type::Int64:
                ctx.m_out << " = 0";
                break;
            case Parser::PrimitiveType::Type::Double:
                ctx.m_out << " = 0.0";
                break;
            }
        }
        else if (m_allEnums.contains(typeName))
        {
            const auto eit = std::find_if(
                enumerations.constBegin(),
                enumerations.constEnd(),
                [&typeName](const auto & enumeration)
                {
                    return typeName == enumeration.m_name;
                });

            if (eit != enumerations.constEnd())
            {
                const auto & values = eit->m_values;
                if (!values.isEmpty()) {
                    ctx.m_out << " = " << typeName << "::"
                        << values.front().first;
                }
            }
        }

        ctx.m_out << ";" << ln;
    }

    ctx.m_out << "};" << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypeImplCpp(
    const Parser::Structure & s, const QString & outPath,
    const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral("Impl.cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        fileSection + QStringLiteral("/impl"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#include \"" << s.m_name << "Impl.h\"" << ln << ln;
    ctx.m_out << "#include \"../../Impl.h\"" << ln << ln;
    ctx.m_out << "#include <QTextStream>" << ln;

    const bool isTypeWithLocalId = structContainsLocalIdField(s);
    if (isTypeWithLocalId) {
        ctx.m_out << "#include <QUuid>" << ln;
    }

    ctx.m_out << ln;
    writeNamespaceBegin(ctx);

    if (isTypeWithLocalId) {
        ctx.m_out << s.m_name << "::Impl::Impl()" << ln
            << "{" << ln;

        ctx.m_out << indent << "m_localId = QUuid::createUuid().toString();"
            << ln
            << indent << "// Remove curvy braces" << ln
            << indent << "m_localId.remove(m_localId.size() - 1, 1);" << ln
            << indent << "m_localId.remove(0, 1);" << ln;

        ctx.m_out << "}" << ln << ln;
    }

    writeTypeImplPrintDefinition(ctx.m_out, s);
    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateSerializationJsonHeader(
    const Parser::Structure & s, const QString & outPath)
{
    const QString fileName = s.m_name + QStringLiteral(".h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("serialization/json"));

    const bool isException = m_allExceptions.contains(s.m_name);

    auto additionalIncludes = QStringList{} << QStringLiteral("<QJsonObject>")
        << QStringLiteral("<optional>");

    {
        QString typeHeader;
        QTextStream strm{&typeHeader};
        strm << "<qevercloud/";
        if (isException) {
            strm << "exceptions";
        }
        else {
            strm << "types";
        }
        strm << "/" << s.m_name << ".h>";

        additionalIncludes << typeHeader;
    }

    sortIncludes(additionalIncludes);

    static const QString section = QStringLiteral("serialization/json");

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Public, section);

    ctx.m_out << "[[nodiscard]] QEVERCLOUD_EXPORT QJsonObject "
        << "serializeToJson(" << ln << "    const " << s.m_name << " & value);"
        << ln << ln;

    ctx.m_out << "[[nodiscard]] QEVERCLOUD_EXPORT bool deserializeFromJson("
        << ln << "    const QJsonObject & object, " << s.m_name << " & value);"
        << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName, {}, {}, section);
}

void Generator::generateSerializationJsonCpp(
    const Parser::Structure & s, const Parser::Enumerations & enumerations,
    const QString & outPath)
{
    const QString fileName = s.m_name + QStringLiteral(".cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("serialization/json"));

    ctx.m_out << disclaimer << ln;

    ctx.m_out << "#include <qevercloud/serialization/json/" << s.m_name
        << ".h>" << ln << ln;

    auto dependentQEverCloudTypeNames = dependentTypeNames(s);
    dependentQEverCloudTypeNames.removeOne(QStringLiteral("QByteArray"));
    dependentQEverCloudTypeNames.removeOne(QStringLiteral("QString"));

    std::sort(
        dependentQEverCloudTypeNames.begin(),
        dependentQEverCloudTypeNames.end());

    for (const auto & typeName: qAsConst(dependentQEverCloudTypeNames))
    {
        ctx.m_out << "#include <qevercloud/serialization/json/" << typeName
            << ".h>" << ln;
    }

    if (!dependentQEverCloudTypeNames.isEmpty()) {
        ctx.m_out << ln;
    }

    const bool needsToRangeHeader =
        [&]
        {
            if (structContainsLocalFields(s)) {
                return true;
            }

            for (const auto & field: s.m_fields)
            {
                if (dynamic_cast<Parser::MapType*>(field.m_type.get())) {
                    return true;
                }
            }

            return false;
        }();

    if (needsToRangeHeader) {
        ctx.m_out << "#include <qevercloud/utility/ToRange.h>" << ln << ln;
    }

    ctx.m_out << "#include <QJsonArray>" << ln << ln;
    ctx.m_out << "#include <limits>" << ln << ln;

    writeNamespaceBegin(ctx);

    Parser::Enumerations enums;
    for (const auto & field: s.m_fields)
    {
        const auto fieldValueType = [&] () -> std::shared_ptr<Parser::Type>
        {
            if (const auto * listType =
                dynamic_cast<Parser::ListType*>(field.m_type.get()))
            {
                return listType->m_valueType;
            }
            else if (const auto * setType =
                     dynamic_cast<Parser::SetType*>(field.m_type.get()))
            {
                return setType->m_valueType;
            }
            else if (const auto * mapType =
                     dynamic_cast<Parser::MapType*>(field.m_type.get()))
            {
                return mapType->m_valueType;
            }

            return field.m_type;
        }();

        const auto typeName = typeToStr(fieldValueType, {});
        const auto actualTypeName = aliasedTypeName(typeName);

        const auto it = std::find_if(
            enumerations.begin(),
            enumerations.end(),
            [&actualTypeName](const Parser::Enumeration & e)
            {
                return actualTypeName == e.m_name;
            });
        if (it != enumerations.end()) {
            enums.push_back(*it);
        }
    }

    enums.erase(
        std::unique(
            enums.begin(),
            enums.end(),
            [](const Parser::Enumeration & lhs, const Parser::Enumeration & rhs)
            {
                return lhs.m_name == rhs.m_name;
            }),
        enums.end());

    if (!enums.empty())
    {
        ctx.m_out << "namespace {" << ln << ln;

        for (const auto & e: qAsConst(enums)) {
            generateSerializationJsonEnumSafeCastMethod(e, ctx);
        }

        ctx.m_out << "} // namespace" << ln << ln;
    }

    generateSerializeToJsonMethod(s, ctx);
    generateDeserializeFromJsonMethod(s, ctx);
    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateSerializationJsonEnumSafeCastMethod(
    const Parser::Enumeration & e, OutputFileContext & ctx)
{
    ctx.m_out << "[[nodiscard]] std::optional<" << e.m_name
        << "> safeCast" << capitalize(e.m_name)
        << "ToEnum(" << ln
        << indent << "const qint64 value) noexcept" << ln;

    ctx.m_out << "{" << ln;

    ctx.m_out << indent << "switch (value)" << ln
        << indent << "{" << ln;

    for (const auto & pair: e.m_values)
    {
        ctx.m_out << indent << "case static_cast<qint64>(" << e.m_name
            << "::" << pair.first << "):" << ln;

        ctx.m_out << indent << indent << "return " << e.m_name
            << "::" << pair.first << ";" << ln;
    }

    ctx.m_out << indent << "default:" << ln
        << indent << indent << "return std::nullopt;" << ln
        << indent << "}" << ln
        << "}" << ln << ln;
}

void Generator::generateSerializeToJsonMethod(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    ctx.m_out << "QJsonObject serializeToJson(const "
        << s.m_name << " & value)" << ln
        << "{" << ln;

    ctx.m_out << indent << "QJsonObject object;" << ln << ln;

    const auto processItem = [&](const std::shared_ptr<Parser::Type> & type,
            const QString & itemName)
        {
            const auto typeName = typeToStr(type, {});
            const auto actualTypeName = aliasedTypeName(typeName);

            if (actualTypeName == typeName &&
                dynamic_cast<Parser::IdentifierType*>(type.get()))
            {
                if (m_allEnums.contains(actualTypeName))
                {
                    ctx.m_out << "QString::number(static_cast<qint64>("
                        << itemName << "));" << ln;
                }
                else
                {
                    ctx.m_out << "serializeToJson(" << itemName << ");" << ln;
                }
            }
            else if (dynamic_cast<Parser::ByteArrayType*>(type.get()))
            {
                ctx.m_out << "QString::fromUtf8(" << itemName << ".toBase64());"
                    << ln;
            }
            else
            {
                if (actualTypeName == QStringLiteral("QVariant")) {
                    ctx.m_out << "QJsonValue::fromVariant(" << itemName
                        << ");" << ln;
                }
                else if (actualTypeName == QStringLiteral("qint64") ||
                         actualTypeName == QStringLiteral("quint64"))
                {
                    ctx.m_out << "QString::number(" << itemName << ");" << ln;
                }
                else {
                    ctx.m_out << itemName << ";" << ln;
                }
            }
        };

    for (const auto & field: s.m_fields)
    {
        const auto * listType =
            dynamic_cast<Parser::ListType*>(field.m_type.get());

        const auto * setType =
            dynamic_cast<Parser::SetType*>(field.m_type.get());

        const auto * mapType =
            dynamic_cast<Parser::MapType*>(field.m_type.get());

        const auto * hashType =
            dynamic_cast<Parser::HashType*>(field.m_type.get());

        if (listType || setType)
        {
            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << indent << "if (value." << field.m_name << "())"
                    << ln;
            }

            ctx.m_out << indent << "{" << ln;
            ctx.m_out << indent << indent << "QJsonArray array;" << ln;

            ctx.m_out << indent << indent << "for (const auto & v: qAsConst(";

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "*";
            }

            ctx.m_out << "value." << field.m_name << "()))" << ln
                << indent << indent << "{" << ln;

            ctx.m_out << indent << indent << indent << "array << ";

            processItem(
                listType ? listType->m_valueType : setType->m_valueType,
                QStringLiteral("v"));

            ctx.m_out << indent << indent << "}" << ln << ln;

            ctx.m_out << indent << indent << "object[QStringLiteral(\""
                << field.m_name << "\")] = array;" << ln;

            ctx.m_out << indent << "}" << ln << ln;
        }
        else if (mapType || hashType)
        {
            const auto & keyType =
                (mapType ? mapType->m_keyType : hashType->m_keyType);

            const auto & valueType =
                (mapType ? mapType->m_valueType : hashType->m_valueType);

            const auto keyTypeName = typeToStr(keyType, {});
            const auto actualKeyTypeName = aliasedTypeName(keyTypeName);
            if (actualKeyTypeName != QStringLiteral("QString")) {
                throw std::runtime_error{
                    "Only strings are supported as keys for maps"};
            }

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << indent << "if (value." << field.m_name << "())"
                    << ln;
            }

            ctx.m_out << indent << "{" << ln;
            ctx.m_out << indent << indent << "QJsonObject subobject;" << ln;

            ctx.m_out << indent << indent << "for (auto it: toRange("
                << "qAsConst(";

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "*";
            }

            ctx.m_out << "value." << field.m_name << "())))" << ln
                << indent << indent << "{" << ln;

            ctx.m_out << indent << indent << indent << "subobject[it.key()] = ";

            processItem(valueType, QStringLiteral("it.value()"));

            ctx.m_out << indent << indent << "}" << ln;
            ctx.m_out << indent << indent << "object[QStringLiteral(\""
                << field.m_name << "\")] = subobject;" << ln;

            ctx.m_out << indent << "}" << ln << ln;
        }
        else
        {
            const auto fieldTypeName = typeToStr(field.m_type, field.m_name);
            const auto actualFieldTypeName = aliasedTypeName(fieldTypeName);

            if (field.m_required ==
                Parser::Field::RequiredFlag::Optional)
            {
                ctx.m_out << indent << "if (value." << field.m_name
                    << "()) {" << ln;
                ctx.m_out << indent << indent;
            }
            else
            {
                ctx.m_out << indent;
            }

            ctx.m_out << "object[QStringLiteral(\""
                << field.m_name << "\")] = ";

            const bool isEnum =
                (actualFieldTypeName == fieldTypeName &&
                 m_allEnums.contains(fieldTypeName));

            const bool isComplexType =
                !isEnum &&
                (actualFieldTypeName == fieldTypeName) &&
                static_cast<bool>(
                    dynamic_cast<Parser::IdentifierType*>(field.m_type.get()));

            const bool isByteArray =
                !isEnum &&
                !isComplexType &&
                static_cast<bool>(
                    dynamic_cast<Parser::ByteArrayType*>(field.m_type.get()));

            const bool isLargeInt =
                !isEnum &&
                ((actualFieldTypeName == QStringLiteral("qint64") ||
                 actualFieldTypeName == QStringLiteral("quint64")));

            if (isEnum) {
                ctx.m_out << "QString::number(static_cast<qint64>(";
            }
            else if (isComplexType) {
                ctx.m_out << "serializeToJson(";
            }
            else if (isByteArray) {
                ctx.m_out << "QString::fromUtf8(";
            }
            else if (isLargeInt) {
                ctx.m_out << "QString::number(";
            }

            if (!isByteArray && (field.m_required ==
                Parser::Field::RequiredFlag::Optional))
            {
                ctx.m_out << "*";
            }

            ctx.m_out << "value." << field.m_name << "()";

            if (isEnum) {
                ctx.m_out << "))";
            }
            else if (isComplexType) {
                ctx.m_out << ")";
            }
            else if (isByteArray) {
                if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                    ctx.m_out << "->";
                }
                else {
                    ctx.m_out << ".";
                }
                ctx.m_out << "toBase64())";
            }
            else if (isLargeInt) {
                ctx.m_out << ")";
            }

            ctx.m_out << ";" << ln;

            if (field.m_required ==
                Parser::Field::RequiredFlag::Optional)
            {
                ctx.m_out << indent << "}" << ln << ln;
            }
        }
    }

    ctx.m_out << indent << "return object;" << ln
        << "}" << ln << ln;
}

void Generator::generateDeserializeFromJsonMethod(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    ctx.m_out << "bool deserializeFromJson("
        << "const QJsonObject & object, " << s.m_name << " & value)" << ln
        << "{" << ln;

    using ValueSetter = std::function<QString(QString)>;

    const auto processValue = [&](
        const std::shared_ptr<Parser::Type> & type, const QString & itemName,
        const QString & indentStr, const ValueSetter setter)
    {
        const auto typeName = typeToStr(type, {});
        const auto actualTypeName = aliasedTypeName(typeName);

        // In json all numbers except 64 bit integers are stored as doubles so
        // for int of size < 64 fields need to use cast but need to do it
        // cautiously, taking the type limits into account
        const auto processIntegerValue = [&](const QString & intTypeName)
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isDouble()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "const auto d = " << itemName << ".toDouble();" << ln;

            ctx.m_out << indentStr << indent
                << "if ((d >= static_cast<double>("
                << "std::numeric_limits<" << intTypeName << ">::min())) &&"
                << ln
                << indentStr << indent << "    "
                << "(d <= static_cast<double>("
                << "std::numeric_limits<" << intTypeName << ">::max())))" << ln
                << indentStr << indent << "{" << ln;

            QString valueToSet;
            {
                QTextStream strm{&valueToSet};
                strm << "static_cast<" << intTypeName << ">(d)";
            }

            ctx.m_out << indentStr << indent
                << indent << setter(valueToSet) << ln;

            ctx.m_out << indentStr << indent << "}" << ln
                << indentStr << indent << "else {" << ln
                << indentStr << indent << indent << "return false;" << ln
                << indentStr << indent << "}" << ln
                << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        };

        // All 64 bit integers are stored in json as strings because otherwise
        // converting them to double and back has proved to lose precision
        const auto processLargeIntegerValue = [&](const QString & intTypeName)
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isString()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "const auto s = " << itemName << ".toString();" << ln;

            ctx.m_out << indentStr << indent
                << "bool conversionResult = false;" << ln;

            ctx.m_out << indentStr << indent
                << intTypeName << " i = s.to"
                << (intTypeName.startsWith(QStringLiteral("quint"))
                    ? "UlongLong"
                    : "LongLong")
                << "(&conversionResult);" << ln;

            ctx.m_out << indentStr << indent
                << "if (!conversionResult) {" << ln
                << indentStr << indent << indent << "return false;" << ln
                << indentStr << indent << "}" << ln << ln;

            ctx.m_out << indentStr << indent << setter(QStringLiteral("i"))
                << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        };

        if (actualTypeName == QStringLiteral("bool"))
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isBool()) {"
                << ln;

            QString valueToSet;
            {
                QTextStream strm{&valueToSet};
                strm << itemName << ".toBool()";
            }

            ctx.m_out << indentStr << indent << setter(valueToSet) << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
        else if (actualTypeName == QStringLiteral("qint8"))
        {
            processIntegerValue(QStringLiteral("qint8"));
        }
        else if (actualTypeName == QStringLiteral("quint8"))
        {
            processIntegerValue(QStringLiteral("quint8"));
        }
        else if (actualTypeName == QStringLiteral("qint16"))
        {
            processIntegerValue(QStringLiteral("qint16"));
        }
        else if (actualTypeName == QStringLiteral("quint16"))
        {
            processIntegerValue(QStringLiteral("quint16"));
        }
        else if (actualTypeName == QStringLiteral("qint32"))
        {
            processIntegerValue(QStringLiteral("qint32"));
        }
        else if (actualTypeName == QStringLiteral("quint32"))
        {
            processLargeIntegerValue(QStringLiteral("quint32"));
        }
        else if (actualTypeName == QStringLiteral("qint64"))
        {
            processLargeIntegerValue(QStringLiteral("qint64"));
        }
        else if (actualTypeName == QStringLiteral("double"))
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isDouble()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "const auto d = " << itemName << ".toDouble();" << ln;

            ctx.m_out << indentStr << indent << setter(QStringLiteral("d"))
                << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
        else if (actualTypeName == QStringLiteral("QString"))
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isString()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "auto s = " << itemName << ".toString();" << ln;

            ctx.m_out << indentStr << indent
                << setter(QStringLiteral("std::move(s)")) << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
        else if (actualTypeName == QStringLiteral("QByteArray"))
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isString()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "auto s = " << itemName << ".toString();" << ln;

            ctx.m_out << indentStr << indent
                << setter(QStringLiteral("QByteArray::fromBase64(s.toUtf8())"))
                << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
        else if (actualTypeName == QStringLiteral("QVariant"))
        {
            ctx.m_out << indentStr << "QVariant f = " << itemName
                << ".toVariant();" << ln;

            ctx.m_out << indentStr << setter(QStringLiteral("f")) << ln;
        }
        else if (m_allEnums.contains(actualTypeName))
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isString()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "const auto s = " << itemName << ".toString();" << ln;

            ctx.m_out << indentStr << indent
                << "bool conversionResult = false;" << ln;

            ctx.m_out << indentStr << indent
                << "qint64 i = s.toLongLong(&conversionResult);" << ln;

            ctx.m_out << indentStr << indent
                << "if (!conversionResult) {" << ln
                << indentStr << indent << indent << "return false;" << ln
                << indentStr << indent << "}" << ln << ln;

            ctx.m_out << indentStr << indent << setter(QStringLiteral("i"))
                << ln;

            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
        else
        {
            ctx.m_out << indentStr << "if (" << itemName << ".isObject()) {"
                << ln;

            ctx.m_out << indentStr << indent
                << "auto o = " << itemName << ".toObject();" << ln;

            ctx.m_out << indentStr << indent
                << typeName << " f;" << ln;

            ctx.m_out << indentStr << indent
                << "if (deserializeFromJson(o, f)) {" << ln;

            ctx.m_out << indentStr << indent << indent
                << setter(QStringLiteral("std::move(f)")) << ln;

            ctx.m_out << indentStr << indent << "}" << ln;
            ctx.m_out << indentStr << indent << "else {" << ln;
            ctx.m_out << indentStr << indent << indent << "return false;" << ln;
            ctx.m_out << indentStr << indent << "}" << ln;
            ctx.m_out << indentStr << "}" << ln
                << indentStr << "else {" << ln
                << indentStr << indent << "return false;" << ln
                << indentStr << "}" << ln;
        }
    };

    for (const auto & field: s.m_fields)
    {
        const auto * listType =
            dynamic_cast<Parser::ListType*>(field.m_type.get());

        const auto * setType =
            dynamic_cast<Parser::SetType*>(field.m_type.get());

        const auto mapType =
            std::dynamic_pointer_cast<Parser::MapType>(field.m_type);

        const auto hashType =
            std::dynamic_pointer_cast<Parser::HashType>(field.m_type);

        if (listType || setType)
        {
            ctx.m_out << indent << "if (object.contains(QStringLiteral(\""
                << field.m_name << "\"))) {" << ln;

            ctx.m_out << indent << indent
                << "const auto v = object[QStringLiteral(\""
                << field.m_name << "\")];" << ln;

            ctx.m_out << indent << indent << "if (v.isArray()) {" << ln;
            ctx.m_out << indent << indent << indent
                << "const auto a = v.toArray();" << ln;

            ctx.m_out << indent << indent << indent
                << typeToStr(field.m_type, {}) << " values;" << ln;

            ctx.m_out << indent << indent << indent
                << "for (const auto & item: qAsConst(a)) {" << ln;

            const QString indentStr = [&]
            {
                QString indentStr;
                QTextStream strm{&indentStr};
                for (int i = 0; i < 4; ++i) {
                    strm << indent;
                }
                return indentStr;
            }();

            const auto & valueType =
                (listType ? listType->m_valueType : setType->m_valueType);

            const auto valueTypeName = typeToStr(valueType, {});
            const auto actualValueTypeName = aliasedTypeName(valueTypeName);

            if (m_allEnums.contains(actualValueTypeName))
            {
                processValue(
                    valueType, QStringLiteral("item"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};
                        strm << "const auto e = safeCast"
                            << capitalize(actualValueTypeName)
                            << "ToEnum(" << value << ");" << ln;

                        strm << indentStr << indent
                            << "if (e) {" << ln;

                        strm << indentStr << indent << indent;

                        if (listType) {
                            strm << "values.push_back(*e);" << ln;
                        }
                        else {
                            strm << "values.insert(*e);" << ln;
                        }

                        strm << indentStr << indent
                            << "}" << ln;

                        strm << indentStr << indent
                            << "else {" << ln;

                        strm << indentStr << indent << indent
                            << "return false;" << ln;

                        strm << indentStr << indent
                            << "}";

                        return str;
                    });
            }
            else
            {
                processValue(
                    valueType, QStringLiteral("item"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};
                        if (listType) {
                            strm << "values.push_back(" << value << ");";
                        }
                        else {
                            strm << "values.insert(" << value << ");";
                        }
                        return str;
                    });
            }

            ctx.m_out << indent << indent << indent << "}" << ln;

            ctx.m_out << indent << indent << indent
                << "value." << fieldSetterName(field)
                << "(std::move(values));" << ln;

            ctx.m_out << indent << indent << "}" << ln;
            ctx.m_out << indent << indent << "else {" << ln
                << indent << indent << indent << "return false;" << ln
                << indent << indent << "}" << ln;
            ctx.m_out << indent << "}" << ln << ln;
        }
        else if (mapType || hashType)
        {
            const auto & keyType =
                (mapType ? mapType->m_keyType : hashType->m_keyType);

            const auto & valueType =
                (mapType ? mapType->m_valueType : hashType->m_valueType);

            const auto keyTypeName = typeToStr(keyType, {});
            const auto actualKeyTypeName = aliasedTypeName(keyTypeName);
            if (actualKeyTypeName != QStringLiteral("QString")) {
                throw std::runtime_error{
                    "Only strings are supported as keys for maps"};
            }

            const auto mapTypeName =
                (mapType
                 ? typeToStr(mapType, {})
                 : typeToStr(hashType, {}));

            ctx.m_out << indent << "if (object.contains(QStringLiteral(\""
                << field.m_name << "\"))) {" << ln;

            ctx.m_out << indent << indent << "auto v = object[QStringLiteral(\""
                << field.m_name << "\")];" << ln;

            ctx.m_out << indent << indent <<
                "if (v.isObject()) {" << ln;

            ctx.m_out << indent << indent << indent
                << "const auto o = v.toObject();" << ln;

            ctx.m_out << indent << indent << indent
                << mapTypeName << " map;" << ln;

            ctx.m_out << indent << indent << indent
                << "for (auto it: toRange(qAsConst(o))) {" << ln;

            const QString indentStr = [&]
            {
                QString indentStr;
                QTextStream strm{&indentStr};
                for (int i = 0; i < 4; ++i) {
                    strm << indent;
                }
                return indentStr;
            }();

            const auto valueTypeName = typeToStr(valueType, {});
            const auto actualValueTypeName = aliasedTypeName(valueTypeName);

            if (m_allEnums.contains(actualValueTypeName))
            {
                processValue(
                    valueType, QStringLiteral("it.value()"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};

                        strm << "const auto e = safeCast"
                            << capitalize(actualValueTypeName)
                            << "ToEnum(" << value << ");" << ln;

                        strm << indentStr << indent
                            << "if (e) {" << ln;

                        strm << indentStr << indent << indent
                            << "map[it.key()] = *e;" << ln;

                        strm << indentStr << indent
                            << "}" << ln;

                        strm << indentStr << indent
                            << "else {" << ln;

                        strm << indentStr << indent << indent
                            << "return false;" << ln;

                        strm << indentStr << indent
                            << "}";

                        return str;
                    });
            }
            else
            {
                processValue(
                    valueType, QStringLiteral("it.value()"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};
                        strm << "map[it.key()] = " << value << ";";
                        return str;
                    });
            }

            ctx.m_out << indent << indent << indent << "}" << ln;

            ctx.m_out << indent << indent << indent
                << "value." << fieldSetterName(field)
                << "(std::move(map));" << ln;

            ctx.m_out << indent << indent << "}" << ln
                << indent << indent << "else {" << ln
                << indent << indent << indent << "return false;" << ln
                << indent << indent << "}" << ln;
            ctx.m_out << indent << "}" << ln << ln;
        }
        else
        {
            ctx.m_out << indent << "if (object.contains(QStringLiteral(\""
                << field.m_name << "\"))) {" << ln;

            ctx.m_out << indent << indent
                << "const auto v = object[QStringLiteral(\""
                << field.m_name << "\")];" << ln;

            const QString indentStr = [&]
            {
                QString indentStr;
                QTextStream strm{&indentStr};
                for (int i = 0; i < 2; ++i) {
                    strm << indent;
                }
                return indentStr;
            }();

            const auto typeName = typeToStr(field.m_type, field.m_name);
            const auto actualTypeName = aliasedTypeName(typeName);
            if (m_allEnums.contains(actualTypeName))
            {
                processValue(
                    field.m_type, QStringLiteral("v"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};
                        strm << "const auto e = safeCast"
                            << capitalize(actualTypeName) << "ToEnum("
                            << value << ");" << ln;

                        strm << indent << indent << indent
                            << "if (e) {" << ln;

                        strm << indent << indent << indent << indent
                            << "value." << fieldSetterName(field)
                            << "(*e);" << ln;

                        strm << indent << indent << indent << "}"
                            << ln;

                        strm << indent << indent << indent << "else {"
                            << ln;

                        strm << indent << indent << indent
                            << indent << "return false;" << ln;

                        strm << indent << indent << indent << "}";
                        return str;
                    });
            }
            else
            {
                processValue(
                    field.m_type, QStringLiteral("v"), indentStr,
                    [&](const QString & value)
                    {
                        QString str;
                        QTextStream strm{&str};
                        strm << "value." << fieldSetterName(field)
                            << "(" << value << ");";
                        return str;
                    });
            }

            ctx.m_out << indent << "}" << ln << ln;
        }
    }

    ctx.m_out << indent << "return true;" << ln
        << "}" << ln << ln;
}

void Generator::generateSerializationJsonTestHeader(
    const Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("TestSerializationJson.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Test);

    const auto additionalIncludes =
        QStringList{} << QStringLiteral("<QObject>");

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "class SerializationJsonTester: public QObject" << ln
        << "{" << ln
        << "    Q_OBJECT" << ln
        << "public:" << ln
        << "    explicit SerializationJsonTester(QObject * parent = "
        << "nullptr);" << ln << ln;

    ctx.m_out << "private Q_SLOTS:" << ln;

    const auto structsAndExceptions = sortedStructsAndExceptions(parser);
    for (const auto & s: qAsConst(structsAndExceptions))
    {
        ctx.m_out << "    void shouldSerializeAndDeserialize" << s.m_name
            << "();" << ln;
    }

    ctx.m_out << "};" << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateSerializationJsonTestCpp(
    const Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList{}
        << QStringLiteral("RandomDataGenerators.h")
        << QStringLiteral("<QtTest/QtTest>");

    const auto structsAndExceptions = sortedStructsAndExceptions(parser);
    for (const auto & s: qAsConst(structsAndExceptions)) {
        additionalIncludes << QStringLiteral("<qevercloud/serialization/json/")
            + s.m_name + QStringLiteral(".h>");
    }

    sortIncludes(additionalIncludes);

    const QString fileName = QStringLiteral("TestSerializationJson.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderBody(
        ctx,
        QStringLiteral("TestSerializationJson.h"),
        additionalIncludes,
        HeaderKind::Test);

    ctx.m_out << "SerializationJsonTester::SerializationJsonTester("
        << "QObject * parent) :" << ln
        << "    QObject(parent)" << ln
        << "{}" << ln << ln;

    for (const auto & s: qAsConst(structsAndExceptions))
    {
        ctx.m_out << "void SerializationJsonTester::"
            << "shouldSerializeAndDeserialize" << s.m_name
            << "()" << ln
            << "{" << ln
            << indent << "const auto original = generateRandom" << s.m_name
            << "();" << ln
            << indent << "const auto object = serializeToJson(original);" << ln
            << indent << s.m_name << " deserialized;" << ln
            << indent << "const bool res = deserializeFromJson("
            << "object, deserialized);" << ln
            << indent << "if (deserialized != original) {" << ln
            << indent << indent
            << "qWarning() << \"Serialization/deserialization failed: "
            << "original = \"" << ln
            << indent << indent << indent
            << "<< original << \"\\nDeserialized = \""
            << " << deserialized" << ln
            << indent << indent << indent
            << "<< \"\\nJSON: \"" << ln
            << indent << indent << indent
            << "<< QJsonDocument(object).toJson(QJsonDocument::Indented);" << ln
            << indent << "}" << ln
            << indent << "QVERIFY(deserialized == original);" << ln
            << "}" << ln << ln;
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateExceptionClassWhatMethodDefinition(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    ctx.m_out << "const char * " << s.m_name << "::what() const noexcept"
        << ln << "{" << ln
        << indent << "return EvernoteException::what();" << ln
        << "}" << ln << ln;
}

void Generator::generateExceptionClassRaiseMethodDefinition(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    ctx.m_out << "void " << s.m_name << "::raise() const" << ln
        << "{" << ln
        << indent << "throw *this;" << ln
        << "}" << ln << ln;
}

void Generator::generateExceptionClassCloneMethodDefinition(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    ctx.m_out << s.m_name << " * " << s.m_name << "::clone() const" << ln
        << "{" << ln
        << indent << "auto e = std::make_unique<" << s.m_name << ">();" << ln;

    for (const auto & f: s.m_fields) {
        ctx.m_out << indent << "e->" << fieldSetterName(f)
            << "(d->m_" << f.m_name << ");" << ln;
    }

    ctx.m_out << indent << "return e.release();" << ln;

    ctx.m_out << "}" << ln << ln;
}

void Generator::generateAllTypeBuildersHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("All.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("types/builders"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_TYPES_BUILDERS_ALL_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_TYPES_BUILDERS_ALL_H" << ln
        << ln;

    const auto & structures = parser.structures();
    QStringList typeBuilders;
    typeBuilders.reserve(structures.size());
    for (const auto & s: structures) {
        typeBuilders << (s.m_name + QStringLiteral("Builder"));
    }

    std::sort(typeBuilders.begin(), typeBuilders.end());

    for (const auto & typeBuilder: qAsConst(typeBuilders)) {
        ctx.m_out << "#include <qevercloud/types/builders/" << typeBuilder
            << ".h>" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_TYPES_BUILDERS_ALL_H" << ln;
}

void Generator::generateTypeBuildersFwdHeader(
    const Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("types/builders"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_TYPES_BUILDERS_FWD_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_TYPES_BUILDERS_FWD_H" << ln
        << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln;

    const auto & structures = parser.structures();
    QStringList typeBuilderClasses;
    typeBuilderClasses.reserve(structures.size());
    for (const auto & s: structures) {
        typeBuilderClasses << s.m_name + QStringLiteral("Builder");
    }

    std::sort(typeBuilderClasses.begin(), typeBuilderClasses.end());

    for (const auto & typeBuilderClass: qAsConst(typeBuilderClasses)) {
        ctx.m_out << "class " << typeBuilderClass << ";" << ln;
    }

    ctx.m_out << ln << "} // namespace qevercloud" << ln << ln;

    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_TYPES_BUILDERS_FWD_H" << ln;
}

void Generator::generateTypeBuilderHeader(
    const Parser::Structure & s, const QString & outPath,
    const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral("Builder.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        fileSection + QStringLiteral("/builders"));

    const QString typeHeader = [&]
    {
        QString result;
        QTextStream strm{&result};

        strm << "<qevercloud/" << fileSection << "/" << s.m_name
            << ".h>";

        return result;
    }();

    QStringList additionalIncludes = QStringList{}
        << typeHeader
        << QStringLiteral("<qevercloud/EDAMErrorCode.h>")
        << QStringLiteral("<qevercloud/types/TypeAliases.h>")
        << QStringLiteral("<QSharedDataPointer>")
        << additionalIncludesForFields(s);

    const bool isExceptionsSection =
        (fileSection == QStringLiteral("exceptions"));

    if (isExceptionsSection) {
        additionalIncludes
            << QStringLiteral("<qevercloud/exceptions/EvernoteException.h>");
    }
    else {
        additionalIncludes
            << QStringLiteral("<qevercloud/exceptions/EverCloudException.h>");
    }

    if (!isExceptionsSection && structContainsLocalFields(s)) {
        additionalIncludes << QStringLiteral("<QHash>")
            << QStringLiteral("<QVariant>");
    }

    const auto deps = dependentTypeNames(s);
    for (const auto & dep: qAsConst(deps)) {
        if (dep == QStringLiteral("QString")) {
            continue;
        }

        if (dep == QStringLiteral("QByteArray")) {
            additionalIncludes.append(QStringLiteral("<QByteArray>"));
            continue;
        }

        const bool isExceptionType = m_allExceptions.contains(dep);
        if (isExceptionType) {
            additionalIncludes.append(
                QStringLiteral("<qevercloud/exceptions/") + dep +
                QStringLiteral(".h>"));
        }
        else {
            additionalIncludes.append(
                QStringLiteral("<qevercloud/types/") + dep +
                QStringLiteral(".h>"));
        }
    }

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Public);

    ctx.m_out << "class QEVERCLOUD_EXPORT " << s.m_name << "Builder" << ln
        << "{" << ln
        << "public:" << ln;

    ctx.m_out << indent << s.m_name << "Builder();" << ln << ln;

    ctx.m_out << indent << s.m_name << "Builder("
        << s.m_name << "Builder && other) noexcept;" << ln << ln;

    ctx.m_out << indent << "~" << s.m_name << "Builder() noexcept;"
        << ln << ln;

    ctx.m_out << indent << s.m_name << "Builder & operator=(" << s.m_name
        << "Builder && other) noexcept;" << ln << ln;

    for(const auto & field: qAsConst(s.m_fields))
    {
        const QString fieldTypeName = fieldTypeToStr(field);

        ctx.m_out << indent << s.m_name << "Builder & "
            << fieldSetterName(field) << "(" << fieldTypeName << " "
            << field.m_name << ");" << ln;
    }

    ctx.m_out << ln;

    ctx.m_out << indent << "[[nodiscard]] " << s.m_name << " build();" << ln
        << ln;

    ctx.m_out << "private:" << ln
        << indent << "class Impl;" << ln
        << indent << "QSharedDataPointer<Impl> d;" << ln;

    ctx.m_out << "};" << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypeBuilderCpp(
    const Parser::Structure & s, const Parser::Enumerations & enumerations,
    const QString & outPath, const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral("Builder.cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        fileSection + QStringLiteral("/builders"));

    ctx.m_out << disclaimer << ln;

    ctx.m_out  << "#include <qevercloud/" << fileSection << "/builders/"
        << s.m_name << "Builder.h>" << ln << ln;

    ctx.m_out << "#include <QSharedData>" << ln << ln;

    writeNamespaceBegin(ctx);

    ctx.m_out << "class Q_DECL_HIDDEN " << s.m_name << "Builder::"
        << "Impl final:" << ln
        << indent << "public QSharedData" << ln
        << "{" << ln
        << "public:" << ln;

    for (const auto & field: qAsConst(s.m_fields))
    {
        const QString fieldTypeName = fieldTypeToStr(field);

        ctx.m_out << indent << fieldTypeName << " m_" << field.m_name;

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            ctx.m_out << ";" << ln;
            continue;
        }

        const auto a = aliasedPrimitiveType(fieldTypeName);
        const auto p = dynamic_cast<Parser::PrimitiveType*>(field.m_type.get());
        if (a || p)
        {
            switch(a ? *a : p->m_type)
            {
            case Parser::PrimitiveType::Type::Bool:
                ctx.m_out << " = false";
                break;
            case Parser::PrimitiveType::Type::Byte:
            case Parser::PrimitiveType::Type::Int16:
            case Parser::PrimitiveType::Type::Int32:
            case Parser::PrimitiveType::Type::Int64:
                ctx.m_out << " = 0";
                break;
            case Parser::PrimitiveType::Type::Double:
                ctx.m_out << " = 0.0";
                break;
            }
        }
        else if (m_allEnums.contains(fieldTypeName))
        {
            const auto eit = std::find_if(
                enumerations.constBegin(),
                enumerations.constEnd(),
                [&fieldTypeName](const auto & enumeration)
                {
                    return fieldTypeName == enumeration.m_name;
                });

            if (eit != enumerations.constEnd())
            {
                const auto & values = eit->m_values;
                if (!values.isEmpty()) {
                    ctx.m_out << " = " << fieldTypeName << "::"
                        << values.front().first;
                }
            }
        }

        ctx.m_out << ";" << ln;
    }

    ctx.m_out << "};" << ln << ln;

    ctx.m_out << s.m_name << "Builder::" << s.m_name << "Builder() :" << ln
        << indent << "d(new " << s.m_name << "Builder::Impl)" << ln
        << "{}" << ln << ln;

    ctx.m_out << s.m_name << "Builder::" << s.m_name << "Builder("
        << s.m_name << "Builder && other) noexcept :" << ln
        << indent << "d{std::move(other.d)}" << ln
        << "{}" << ln << ln;

    ctx.m_out << s.m_name << "Builder::~" << s.m_name << "Builder() noexcept"
        << " = default;" << ln << ln;

    ctx.m_out << s.m_name << "Builder & " << s.m_name << "Builder::operator=("
        << s.m_name << "Builder && other) noexcept" << ln
        << "{" << ln
        << indent << "if (this != &other) {" << ln
        << indent << indent << "d = std::move(other.d);" << ln
        << indent << "}" << ln << ln
        << indent << "return *this;" << ln
        << "}" << ln << ln;

    for (const auto & field: qAsConst(s.m_fields))
    {
        const QString fieldTypeName = fieldTypeToStr(field);

        ctx.m_out << s.m_name << "Builder & " << s.m_name << "Builder::"
            << fieldSetterName(field) << "(" << fieldTypeName
            << " " << field.m_name << ")" << ln
            << "{" << ln
            << indent << "d->m_" << field.m_name << " = ";

        if (isFieldOfPrimitiveType(field, fieldTypeName)) {
            ctx.m_out << field.m_name;
        }
        else {
            ctx.m_out << "std::move(" << field.m_name << ")";
        }

        ctx.m_out << ";" << ln
            << indent << "return *this;" << ln
            << "}" << ln << ln;
    }

    ctx.m_out << s.m_name << " " << s.m_name << "Builder::build()" << ln
        << "{" << ln;

    ctx.m_out << indent << s.m_name << " result;" << ln << ln;

    for(const auto & field: qAsConst(s.m_fields))
    {
        ctx.m_out << indent << "result." << fieldSetterName(field)
            << "(";

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            ctx.m_out << "std::move(d->m_" << field.m_name << "));" << ln;
            continue;
        }

        const QString fieldTypeName = fieldTypeToStr(field);

        if (isFieldOfPrimitiveType(field, fieldTypeName)) {
            ctx.m_out << "d->m_" << field.m_name << ");" << ln;
        }
        else {
            ctx.m_out << "std::move(d->m_" << field.m_name << "));" << ln;
        }
    }

    ctx.m_out << ln;

    for (const auto & field: qAsConst(s.m_fields))
    {
        ctx.m_out << indent << "d->m_" << field.m_name;

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            ctx.m_out << " = {};" << ln;
            continue;
        }

        const QString fieldTypeName = fieldTypeToStr(field);
        if (!isFieldOfPrimitiveType(field, fieldTypeName)) {
            if (fieldTypeName == QStringLiteral("QString")) {
                ctx.m_out << " = QString{};" << ln;
            }
            else if (fieldTypeName == QStringLiteral("Guid")) {
                ctx.m_out << " = Guid{};" << ln;
            }
            else if (fieldTypeName == QStringLiteral("QStringList")) {
                ctx.m_out << " = QStringList{};" << ln;
            }
            else {
                ctx.m_out << " = {};" << ln;
            }
            continue;
        }

        const auto a = aliasedPrimitiveType(fieldTypeName);
        const auto p = dynamic_cast<Parser::PrimitiveType*>(field.m_type.get());
        if (a || p)
        {
            switch(a ? *a : p->m_type)
            {
            case Parser::PrimitiveType::Type::Bool:
                ctx.m_out << " = false";
                break;
            case Parser::PrimitiveType::Type::Byte:
            case Parser::PrimitiveType::Type::Int16:
            case Parser::PrimitiveType::Type::Int32:
            case Parser::PrimitiveType::Type::Int64:
                ctx.m_out << " = 0";
                break;
            case Parser::PrimitiveType::Type::Double:
                ctx.m_out << " = 0.0";
                break;
            }
        }
        else if (m_allEnums.contains(fieldTypeName))
        {
            const auto eit = std::find_if(
                enumerations.constBegin(),
                enumerations.constEnd(),
                [&fieldTypeName](const auto & enumeration)
                {
                    return fieldTypeName == enumeration.m_name;
                });

            if (eit != enumerations.constEnd())
            {
                const auto & values = eit->m_values;
                if (!values.isEmpty()) {
                    ctx.m_out << " = " << fieldTypeName << "::"
                        << values.front().first;
                }
            }
        }

        ctx.m_out << ";" << ln;
    }

    ctx.m_out << ln
        << indent << "return result;" << ln
        << "}" << ln << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateAllExceptionBuildersHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("All.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("exceptions/builders"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_ALL_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_ALL_H" << ln
        << ln;

    const auto & exceptions = parser.exceptions();
    QStringList exceptionBuilders;
    exceptionBuilders.reserve(exceptions.size());
    for (const auto & e: exceptions) {
        exceptionBuilders << (e.m_name + QStringLiteral("Builder"));
    }

    std::sort(exceptionBuilders.begin(), exceptionBuilders.end());

    for (const auto & exceptionBuilder: qAsConst(exceptionBuilders)) {
        ctx.m_out << "#include <qevercloud/exceptions/builders/"
            << exceptionBuilder << ".h>" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_ALL_H"
        << ln;
}

void Generator::generateExceptionBuildersFwdHeader(
    const Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("exceptions/builders"));

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_FWD_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_FWD_H" << ln
        << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln;

    const auto & exceptions = parser.exceptions();
    QStringList exceptionBuilderClasses;
    exceptionBuilderClasses.reserve(exceptions.size());
    for (const auto & e: exceptions) {
        exceptionBuilderClasses << e.m_name + QStringLiteral("Builder");
    }

    std::sort(exceptionBuilderClasses.begin(), exceptionBuilderClasses.end());

    for (const auto & exceptionBuilderClass: qAsConst(exceptionBuilderClasses))
    {
        ctx.m_out << "class " << exceptionBuilderClass << ";" << ln;
    }

    ctx.m_out << ln << "} // namespace qevercloud" << ln << ln;

    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_EXCEPTIONS_BUILDERS_FWD_H"
        << ln;
}

void Generator::generateTypeBuildersTestHeader(
    const Parser & parser, const QString & outPath)
{
    const QString fileName =
        QStringLiteral("TestTypeBuilders.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Test);

    const auto additionalIncludes =
        QStringList{} << QStringLiteral("<QObject>");

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "class TypeBuildersTester: public QObject" << ln
        << "{" << ln
        << "    Q_OBJECT" << ln
        << "public:" << ln
        << "    explicit TypeBuildersTester(QObject * parent = "
        << "nullptr);" << ln << ln;

    ctx.m_out << "private Q_SLOTS:" << ln;

    for (const auto & s: parser.structures()) {
        ctx.m_out << "    void shouldBuild" << s.m_name << "();" << ln;
    }

    ctx.m_out << ln;

    for (const auto & s: parser.exceptions()) {
        ctx.m_out << "    void shouldBuild" << s.m_name << "();" << ln;
    }

    ctx.m_out << "};" << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypeBuildersTestCpp(
    const Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList{}
        << QStringLiteral("RandomDataGenerators.h")
        << QStringLiteral("<QtTest/QtTest>");

    for (const auto & s: parser.structures()) {
        additionalIncludes << QStringLiteral("<qevercloud/types/builders/") +
            s.m_name + QStringLiteral("Builder.h>");
    }

    for (const auto & e: parser.exceptions()) {
        additionalIncludes
            << QStringLiteral("<qevercloud/exceptions/builders/") + e.m_name +
            QStringLiteral("Builder.h>");
    }

    sortIncludes(additionalIncludes);

    const QString fileName =
        QStringLiteral("TestTypeBuilders.cpp");

    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderBody(
        ctx,
        QStringLiteral("TestTypeBuilders.h"),
        additionalIncludes,
        HeaderKind::Test);

    ctx.m_out << "TypeBuildersTester::TypeBuildersTester(QObject * parent) :"
        << ln << "    QObject(parent)" << ln
        << "{}" << ln << ln;

    const auto & enumerations = parser.enumerations();

    for (const auto & s: qAsConst(parser.structures())) {
        generateTypeBuildersTestMethod(s, enumerations, ctx, false);
    }

    for (const auto & s: qAsConst(parser.exceptions())) {
        generateTypeBuildersTestMethod(s, enumerations, ctx, true);
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTypeBuildersTestMethod(
    const Parser::Structure & s,
    const QList<Parser::Enumeration> & enumerations,
    OutputFileContext & ctx, bool isException)
{
    ctx.m_out << "void TypeBuildersTester::shouldBuild" << s.m_name
        << "()" << ln
        << "{" << ln;

    ctx.m_out << indent << s.m_name << " value;" << ln;

    for (const auto & field: s.m_fields)
    {
        const auto * mapType =
            dynamic_cast<Parser::MapType*>(field.m_type.get());

        const auto * hashType =
            dynamic_cast<Parser::HashType*>(field.m_type.get());

        if (mapType || hashType)
        {
            ctx.m_out << ln;

            const auto & keyType =
                (mapType ? mapType->m_keyType : hashType->m_keyType);

            const auto & valueType =
                (mapType ? mapType->m_valueType : hashType->m_valueType);

            QString keyTypeName =
                typeToStr(keyType, field.m_name);

            if (const auto * identifierType =
                dynamic_cast<Parser::IdentifierType*>(keyType.get()))
            {
                keyTypeName = clearInclude(identifierType->m_identifier);
            }

            keyTypeName = aliasedTypeName(keyTypeName);

            QString valueTypeName =
                typeToStr(valueType, field.m_name);

            if (const auto * identifierType =
                dynamic_cast<Parser::IdentifierType*>(
                    valueType.get()))
            {
                valueTypeName = clearInclude(identifierType->m_identifier);
            }

            valueTypeName = aliasedTypeName(valueTypeName);

            ctx.m_out << indent
                << typeToStr(field.m_type, field.m_name)
                << " " << field.m_name << ";" << ln;

            ctx.m_out << indent << field.m_name << "[";

            auto enumIt = std::find_if(
                enumerations.begin(),
                enumerations.end(),
                [&] (const Parser::Enumeration & e)
                {
                    return e.m_name == keyTypeName;
                });
            if (enumIt != enumerations.end())
            {
                const Parser::Enumeration & e = *enumIt;
                if (e.m_values.isEmpty()) {
                    throw std::runtime_error(
                        "Detected enumeration without items: " +
                        e.m_name.toStdString());
                }

                ctx.m_out << keyTypeName
                    << "::" << e.m_values[0].first;
            }
            else
            {
                ctx.m_out << getGenerateRandomValueFunction(keyTypeName);
            }

            ctx.m_out << "] = ";

            enumIt = std::find_if(
                enumerations.begin(),
                enumerations.end(),
                [&] (const Parser::Enumeration & e)
                {
                    return e.m_name == valueTypeName;
                });
            if (enumIt != enumerations.end())
            {
                const Parser::Enumeration & e = *enumIt;
                if (e.m_values.isEmpty()) {
                    throw std::runtime_error(
                        "Detected enumeration without items: " +
                        e.m_name.toStdString());
                }

                ctx.m_out << valueTypeName
                    << "::" << e.m_values[0].first;
            }
            else
            {
                ctx.m_out << getGenerateRandomValueFunction(valueTypeName);
            }

            ctx.m_out << ";" << ln;

            ctx.m_out << indent << "value." << fieldSetterName(field)
                << "(std::move(" << field.m_name << "));" << ln << ln;
            continue;
        }

        QString fieldTypeName;
        if (const auto * listType =
            dynamic_cast<Parser::ListType*>(field.m_type.get()))
        {
            fieldTypeName =
                typeToStr(listType->m_valueType, field.m_name);

            if (const auto * identifierType =
                dynamic_cast<Parser::IdentifierType*>(
                    listType->m_valueType.get()))
            {
                fieldTypeName = clearInclude(identifierType->m_identifier);
                fieldTypeName = aliasedTypeName(fieldTypeName);
            }

            ctx.m_out << indent << "value." << fieldSetterName(field)
                << "(QList<" << fieldTypeName << ">{} << ";
        }
        else if (const auto * setType =
                 dynamic_cast<Parser::SetType*>(field.m_type.get()))
        {
            fieldTypeName =
                typeToStr(setType->m_valueType, field.m_name);

            if (const auto * identifierType =
                dynamic_cast<Parser::IdentifierType*>(
                    setType->m_valueType.get()))
            {
                fieldTypeName = clearInclude(identifierType->m_identifier);
            }

            ctx.m_out << indent << "value." << fieldSetterName(field)
                << "(QSet<" << fieldTypeName << ">{} << ";
        }
        else
        {
            fieldTypeName = typeToStr(field.m_type, field.m_name);
            ctx.m_out << indent << "value." << fieldSetterName(field) << "(";
        }

        fieldTypeName = aliasedTypeName(fieldTypeName);

        const auto enumIt = std::find_if(
            enumerations.begin(),
            enumerations.end(),
            [&] (const Parser::Enumeration & e)
            {
                return e.m_name == fieldTypeName;
            });
        if (enumIt != enumerations.end())
        {
            const Parser::Enumeration & e = *enumIt;
            if (e.m_values.isEmpty()) {
                throw std::runtime_error(
                    "Detected enumeration without items: " +
                    e.m_name.toStdString());
            }

            ctx.m_out << fieldTypeName
                << "::" << e.m_values[0].first;
        }
        else
        {
            ctx.m_out
                << getGenerateRandomValueFunction(fieldTypeName);
        }

        ctx.m_out << ");" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << indent << s.m_name << "Builder builder;" << ln;

    for (const auto & field: qAsConst(s.m_fields)) {
        ctx.m_out << indent << "builder." << fieldSetterName(field)
            << "(value." << field.m_name << "());" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << indent << "auto built = builder.build();" << ln;
    ctx.m_out << indent << "QVERIFY(built == value);" << ln;

    ctx.m_out << "}" << ln << ln;
}

void Generator::generateMetaTypesHeader(
    const Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Metatypes.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("types"));

    auto additionalIncludes = QStringList{}
        << QStringLiteral("<qevercloud/exceptions/All.h>")
        << QStringLiteral("<qevercloud/types/All.h>")
        << QStringLiteral("<QMetaType>");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << "/**" << ln
        << " * This function calls qRegisterMetatype for fields of QEverCloud"
        << ln
        << " * types and exceptions so that they can be used in queued "
        << "signal-slot connections." << ln
        << " * Call this function if you need it." << ln
        << " */" << ln;
    ctx.m_out << "QEVERCLOUD_EXPORT void registerMetatypes();" << ln << ln;

    const auto processSingularType =
        [&](const std::shared_ptr<Parser::Type> & type)
        {
            const auto valueTypeName = typeToStr(type, {});
            const auto actualTypeName = aliasedTypeName(valueTypeName);

            if (valueTypeName != actualTypeName) {
                return actualTypeName;
            }

            if (dynamic_cast<Parser::IdentifierType*>(type.get())) {
                return QStringLiteral("qevercloud::") + valueTypeName;
            }

            return valueTypeName;
        };

    auto structsAndExceptions = parser.structures();
    structsAndExceptions << parser.exceptions();

    std::set<QString> dependentOptionalTypeNames;

    struct MapType
    {
        QString m_keyType;
        QString m_valueType;
    };

    std::map<QString, MapType> dependentOptionalMapTypes;

    for (const auto & s: qAsConst(structsAndExceptions))
    {
        for (const auto & f: qAsConst(s.m_fields))
        {
            if (f.m_required != Parser::Field::RequiredFlag::Optional) {
                continue;
            }

            if (const auto * listType =
                dynamic_cast<Parser::ListType*>(f.m_type.get()))
            {
                QString typeName;
                QTextStream strm{&typeName};
                strm << "QList<";
                strm << processSingularType(listType->m_valueType);
                strm << ">";
                dependentOptionalTypeNames.insert(typeName);
            }
            else if (const auto * setType =
                     dynamic_cast<Parser::SetType*>(f.m_type.get()))
            {
                QString typeName;
                QTextStream strm{&typeName};
                strm << "QSet<";
                strm << processSingularType(setType->m_valueType);
                strm << ">";
                dependentOptionalTypeNames.insert(typeName);
            }
            else if (const auto * mapType =
                     dynamic_cast<Parser::MapType*>(f.m_type.get()))
            {
                const auto & keyType = mapType->m_keyType;
                const auto & valueType = mapType->m_valueType;

                auto keyTypeName = processSingularType(keyType);
                auto valueTypeName = processSingularType(valueType);

                QString typeName;
                QTextStream strm{&typeName};
                strm << "QMap<";
                strm << keyTypeName;
                strm << ", ";
                strm << valueTypeName;
                strm << ">";

                dependentOptionalMapTypes[typeName] =
                    MapType{std::move(keyTypeName), std::move(valueTypeName)};
            }
            else
            {
                dependentOptionalTypeNames.insert(
                    processSingularType(f.m_type));
            }
        }
    }

    QStringList extraLinesOutsideNamespace;
    extraLinesOutsideNamespace.reserve(
        dependentOptionalTypeNames.size() +
        dependentOptionalMapTypes.size() * 3 + 1);

    for (const auto & typeName: qAsConst(dependentOptionalTypeNames))
    {
        QString line;
        QTextStream strm{&line};
        strm << "Q_DECLARE_METATYPE(std::optional<" << typeName << ">);";
        extraLinesOutsideNamespace << line;
    }

    extraLinesOutsideNamespace << QString{};

    for (auto it = dependentOptionalMapTypes.begin(),
         end = dependentOptionalMapTypes.end(); it != end; ++it)
    {
        const auto & typeName = it->first;
        const auto & mapType = it->second;

        QString typeAlias;
        {
            QTextStream strm{&typeAlias};
            strm << "Map" << capitalize(mapType.m_keyType)
                << "To" << capitalize(mapType.m_valueType);
        }

        {
            QString line;
            QTextStream strm{&line};
            strm << "using " << typeAlias << " = " << typeName << ";";
            extraLinesOutsideNamespace << line;
        }

        {
            QString line;
            QTextStream strm{&line};
            strm << "Q_DECLARE_METATYPE(std::optional<" << typeAlias << ">);";
            extraLinesOutsideNamespace << line;
        }

        extraLinesOutsideNamespace << QString{};
    }

    if (!dependentOptionalMapTypes.empty() &&
        !extraLinesOutsideNamespace.isEmpty())
    {
        extraLinesOutsideNamespace.erase(
            std::prev(extraLinesOutsideNamespace.end()));
    }

    writeHeaderFooter(ctx.m_out, fileName, {}, extraLinesOutsideNamespace);
}

void Generator::generateMetaTypesCpp(
    const Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Metatypes.cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("types"));

    writeHeaderBody(
        ctx, QStringLiteral("types/Metatypes.h"), {}, HeaderKind::Public, 1);

    ctx.m_out << "void registerMetatypes()" << ln
        << "{" << ln;

    const auto processSingularType =
        [&](const std::shared_ptr<Parser::Type> & type)
        {
            const auto valueTypeName = typeToStr(type, {});
            const auto actualTypeName = aliasedTypeName(valueTypeName);

            if (valueTypeName != actualTypeName) {
                return std::make_pair(actualTypeName, actualTypeName);
            }

            if (dynamic_cast<Parser::IdentifierType*>(type.get())) {
                return std::make_pair(
                    valueTypeName,
                    QStringLiteral("qevercloud::") + valueTypeName);
            }

            return std::make_pair(valueTypeName, valueTypeName);
        };

    auto structsAndExceptions = parser.structures();
    structsAndExceptions << parser.exceptions();

    std::sort(
        structsAndExceptions.begin(),
        structsAndExceptions.end(),
        [](const Parser::Structure & lhs, const Parser::Structure & rhs)
        {
            return lhs.m_name < rhs.m_name;
        });

    std::set<std::pair<QString, QString>> dependentOptionalTypeNames;

    const auto processStruct = [&](const Parser::Structure & s)
    {
        ctx.m_out << indent << "qRegisterMetaType<"
            << s.m_name << ">(\"qevercloud::" << s.m_name << "\");" << ln;

        for (const auto & f: qAsConst(s.m_fields))
        {
            if (f.m_required != Parser::Field::RequiredFlag::Optional) {
                continue;
            }

            if (const auto * listType =
                dynamic_cast<Parser::ListType*>(f.m_type.get()))
            {
                const auto typeNames =
                    processSingularType(listType->m_valueType);

                QString typeName;
                QTextStream strm{&typeName};
                strm << "QList<";
                strm << typeNames.second;
                strm << ">";
                dependentOptionalTypeNames.insert(typeNames);
            }
            else if (const auto * setType =
                     dynamic_cast<Parser::SetType*>(f.m_type.get()))
            {
                const auto typeNames =
                    processSingularType(setType->m_valueType);

                QString typeName;
                QTextStream strm{&typeName};
                strm << "QSet<";
                strm << typeNames.second;
                strm << ">";
                dependentOptionalTypeNames.insert(typeNames);
            }
            else if (const auto * mapType =
                     dynamic_cast<Parser::MapType*>(f.m_type.get()))
            {
                const auto & keyType = mapType->m_keyType;
                const auto & valueType = mapType->m_valueType;

                const auto keyTypeNames = processSingularType(keyType);
                const auto valueTypeNames = processSingularType(valueType);

                QString typeName;
                QTextStream strm{&typeName};
                strm << "QMap<";
                strm << keyTypeNames.second;
                strm << ", ";
                strm << valueTypeNames.second;
                strm << ">";

                dependentOptionalTypeNames.insert(
                    std::make_pair(typeName, typeName));
            }
            else
            {
                dependentOptionalTypeNames.insert(
                    processSingularType(f.m_type));
            }
        }
    };

    const auto structureLessByName =
        [](const Parser::Structure & lhs,
           const Parser::Structure & rhs) noexcept
        {
            return lhs.m_name < rhs.m_name;
        };

    const auto processStructures = [&](Parser::Structures structures)
    {
        std::sort(structures.begin(), structures.end(), structureLessByName);
        for (const auto & s: qAsConst(structures))
        {
            processStruct(s);
        }

        ctx.m_out << ln;
    };

    processStructures(parser.exceptions());
    processStructures(parser.structures());

    for (const auto & typeAlias: qAsConst(parser.typeAliases()))
    {
        ctx.m_out << indent << "qRegisterMetaType<"
            << typeAlias.m_name << ">(\"qevercloud::" << typeAlias.m_name
            << "\");" << ln;
    }

    ctx.m_out << ln;

    for (const auto & typeNames: qAsConst(dependentOptionalTypeNames))
    {
        ctx.m_out << indent << "qRegisterMetaType<std::optional<"
            << typeNames.first << ">>(\"std::optional<" << typeNames.second
            << ">\");" << ln;
    }

    ctx.m_out << "}" << ln << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateServiceHeader(
    const Parser::Service & service, const QString & outPath)
{
    const QString fileName = QStringLiteral("I") + service.m_name +
        QStringLiteral(".h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("services"));

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/DurableService.h>")
        << QStringLiteral("<qevercloud/RequestContext.h>")
        << QStringLiteral("<qevercloud/Constants.h>")
        << QStringLiteral("<qevercloud/Types.h>")
        << QStringLiteral("<qevercloud/services/Fwd.h>")
        << QStringLiteral("<QFuture>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Public);

    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    ctx.m_out << blockSeparator << ln << ln;

    if (!service.m_docComment.isEmpty()) {
        ctx.m_out << service.m_docComment << ln;
    }

    ctx.m_out << "class QEVERCLOUD_EXPORT I" << service.m_name << ln
        << "{" << ln;
    ctx.m_out << "public:" << ln;
    ctx.m_out << "    virtual ~I" << service.m_name << "() = default;" << ln
        << ln;
    ctx.m_out << "    [[nodiscard]] virtual QString "
        << decapitalize(service.m_name) << "Url() const = 0;" << ln;
    ctx.m_out << "    virtual void set" << service.m_name
        << "Url(QString url) = 0;" << ln << ln;

    if (service.m_name == QStringLiteral("NoteStore"))
    {
        ctx.m_out << "    [[nodiscard]] virtual const std::optional<Guid> & "
            << "linkedNotebookGuid() const = 0;"
            << ln
            << "    virtual void setLinkedNotebookGuid("
            << "std::optional<Guid> linkedNotebookGuid) = 0;"
            << ln << ln;
    }

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        if (!func.m_docComment.isEmpty())
        {
            const QStringList lines = func.m_docComment.split(
                QChar::fromLatin1('\n'));

            for(const auto & line: qAsConst(lines)) {
                ctx.m_out << "    " << line << ln;
            }
        }

        const auto funcReturnTypeName = typeToStr(func.m_type, func.m_name);

        ctx.m_out << "    ";
        if (funcReturnTypeName != QStringLiteral("void")) {
            ctx.m_out << "[[nodiscard]] ";
        }

        ctx.m_out << "virtual " << funcReturnTypeName << " " << func.m_name
            << "(";

        if (!func.m_params.isEmpty()) {
            ctx.m_out << ln;
        }

        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_initializer) {
                ctx.m_out << " = " << valueToStr(
                    param.m_initializer, param.m_type,
                    func.m_name + QStringLiteral(", ") + param.m_name);
            }

            ctx.m_out << "," << ln;
        }

        if (!func.m_params.isEmpty()) {
            ctx.m_out << "        ";
        }
        ctx.m_out << "IRequestContextPtr ctx = {}";
        ctx.m_out << ") = 0;" << ln << ln;

        ctx.m_out << "    /** Asynchronous version of @link " << func.m_name
            << " @endlink */" << ln;
        ctx.m_out << "    [[nodiscard]] virtual QFuture<" << funcReturnTypeName
            << "> " << func.m_name << "Async(" << ln;

        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_initializer) {
                ctx.m_out << " = " << valueToStr(
                    param.m_initializer,
                    param.m_type,
                    func.m_name + QStringLiteral(", ") + param.m_name);
            }

            ctx.m_out << "," << ln;
        }

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") = 0;" << ln << ln;
    }

    ctx.m_out << "};" << ln << ln;

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "QEVERCLOUD_EXPORT I" << service.m_name << "Ptr new"
        << service.m_name << "(" << ln;

    ctx.m_out << "    QString " << decapitalize(service.m_name)
        << "Url = {}," << ln;

    if (service.m_name == QStringLiteral("NoteStore"))
    {
        ctx.m_out << "    std::optional<Guid> linkedNotebookGuid"
            << " = {}," << ln;
    }

    ctx.m_out << "    IRequestContextPtr ctx = {}," << ln
        << "    IRetryPolicyPtr retryPolicy = {});" << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateServiceCpp(
    const Parser::Service & service, const QString & outPath)
{
    const QString fileName = service.m_name + QStringLiteral(".cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("services"));

    auto additionalIncludes = QStringList()
        << QStringLiteral("../Types_io.h")
        << QStringLiteral("../Http.h")
        << QStringLiteral("../Future.h")
        << QStringLiteral("<qevercloud/utility/Log.h>")
        << QStringLiteral("<qevercloud/DurableService.h>")
        << QStringLiteral("<algorithm>") << QStringLiteral("<cmath>");

    sortIncludes(additionalIncludes);

    writeHeaderBody(
        ctx,
        QStringLiteral("services/I") + service.m_name + QStringLiteral(".h"),
        additionalIncludes, HeaderKind::Public, 1);

    ctx.m_out << blockSeparator << ln << ln;
    generateServiceClassDeclaration(service, ServiceClassType::NonDurable, ctx);
    generateServiceClassDefinition(service, ctx);

    ctx.m_out << blockSeparator << ln << ln;
    generateServiceClassDeclaration(service, ServiceClassType::Durable, ctx);
    generateDurableServiceClassDefinition(service, ctx);

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "I" << service.m_name << "Ptr new" << service.m_name << "("
        << ln;

    auto serviceName = decapitalize(service.m_name);

    ctx.m_out << "    QString " << serviceName << "Url," << ln;

    const bool isNoteStore = (service.m_name == QStringLiteral("NoteStore"));
    if (isNoteStore)
    {
        ctx.m_out << "    std::optional<Guid> linkedNotebookGuid," << ln;
    }

    ctx.m_out << "    IRequestContextPtr ctx," << ln
        << "    IRetryPolicyPtr retryPolicy)" << ln
        << "{" << ln
        << "    if (ctx && ctx->maxRequestRetryCount() == 0)" << ln
        << "    {" << ln
        << "        return std::make_shared<" << service.m_name
        << ">(std::move(" << serviceName << "Url), ";

    if (isNoteStore) {
        ctx.m_out << "std::move(linkedNotebookGuid), ";
    }

    ctx.m_out << "ctx);" << ln
        << "    }" << ln
        << "    else" << ln
        << "    {" << ln
        << "        if (!retryPolicy) {" << ln
        << "            retryPolicy = newRetryPolicy();" << ln
        << "        }" << ln << ln
        << "        return std::make_shared<Durable" << service.m_name << ">("
        << ln
        << "            std::make_shared<" << service.m_name << ">(std::move("
        << serviceName << "Url), ";

    if (isNoteStore) {
        ctx.m_out << "std::move(linkedNotebookGuid), ";
    }

    ctx.m_out << "ctx)," << ln
        << "            ctx," << ln
        << "            retryPolicy);" << ln
        << "    }" << ln
        << "}" << ln
        << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateAllServicesHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("All.h");
    const QString section = QStringLiteral("services");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, section);

    ctx.m_out << disclaimer << ln;

    const QString guard = getIncludeGuard(fileName, section);

    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;

    const auto & services = parser.services();
    for (const auto & service: qAsConst(services)) {
        ctx.m_out << "#include <qevercloud/services/I"
            << service.m_name << ".h>" << ln;
    }

    for (const auto & service: qAsConst(services)) {
        ctx.m_out << "#include <qevercloud/services/"
            << service.m_name << "Server.h>" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // " << guard << ln;
}

void Generator::generateServicesFwdHeader(
    Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");
    const QString section = QStringLiteral("services");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface, section);

    ctx.m_out << disclaimer << ln;

    const QString guard = getIncludeGuard(fileName, section);

    ctx.m_out << "#include <memory>" << ln << ln;

    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln;

    const auto & services = parser.services();
    QStringList serviceClasses;
    serviceClasses.reserve(services.size() * 2);
    for (const auto & service: qAsConst(services)) {
        serviceClasses << (QStringLiteral("I") + service.m_name);
        serviceClasses << (service.m_name + QStringLiteral("Server"));
    }

    std::sort(serviceClasses.begin(), serviceClasses.end());

    for (const auto & serviceClass: qAsConst(serviceClasses)) {
        ctx.m_out << "class " << serviceClass << ";" << ln;

        if (!serviceClass.endsWith(QStringLiteral("Server"))) {
            ctx.m_out << "using " << serviceClass << "Ptr = "
                << "std::shared_ptr<" << serviceClass << ">;" << ln << ln;
        }
    }
    ctx.m_out << ln;

    writeNamespaceEnd(ctx.m_out);
    ctx.m_out << ln;
    ctx.m_out << "#endif // " << guard << ln;
}

void Generator::generateFwdHeader(const QString & outPath)
{
    const QString fileName = QStringLiteral("Fwd.h");

    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    ctx.m_out << disclaimer << ln;

    const QString guard = getIncludeGuard(fileName, QString{});

    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;
    ctx.m_out << "#include \"exceptions/Fwd.h\"" << ln;
    ctx.m_out << "#include \"services/Fwd.h\"" << ln;
    ctx.m_out << "#include \"types/Fwd.h\"" << ln << ln;
    ctx.m_out << "#include <memory>" << ln << ln;

    ctx.m_out << "namespace qevercloud {" << ln << ln
        << "class IRequestContext;" << ln
        << "using IRequestContextPtr = std::shared_ptr<IRequestContext>;" << ln
        << ln
        << "} // namespace qevercloud" << ln << ln;

    ctx.m_out << "#endif // " << guard << ln;
}

void Generator::generateServerHeader(
    const Parser::Service & service, const QString & outPath)
{
    const QString fileName = service.m_name + QStringLiteral("Server.h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Interface,
        QStringLiteral("services"));

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/RequestContext.h>")
        << QStringLiteral("<qevercloud/Constants.h>")
        << QStringLiteral("<qevercloud/Types.h>")
        << QStringLiteral("<QObject>")
        << QStringLiteral("<exception>")
        << QStringLiteral("<functional>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);
    generateServerClassDeclaration(service, ctx);
    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateServerCpp(
    const Parser::Service & service, const QString & outPath)
{
    const QString fileName = service.m_name + QStringLiteral("Server.cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation,
        QStringLiteral("services"));

    auto additionalIncludes = QStringList() << QStringLiteral("../Thrift.h")
        << QStringLiteral("../Types_io.h")
        << QStringLiteral("<qevercloud/utility/Log.h>");

    sortIncludes(additionalIncludes);

    writeHeaderBody(
        ctx,
        QStringLiteral("services/") + service.m_name +
        QStringLiteral("Server.h"),
        additionalIncludes, HeaderKind::Public, 1);

    ctx.m_out << "namespace {" << ln << ln;
    generateServerHelperFunctions(service, ctx);
    ctx.m_out << "} // namespace" << ln << ln;

    generateServerClassDefinition(service, ctx);
    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTestServerHeader(
    const Parser::Service & service, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../SocketHelpers.h") << QStringLiteral("<QObject>");

    sortIncludes(additionalIncludes);

    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    const QString fileName = QStringLiteral("Test") + service.m_name +
        QStringLiteral(".h");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Test, QStringLiteral("services"));

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "class " << service.m_name << "Tester: public QObject" << ln
        << "{" << ln
        << "    Q_OBJECT" << ln
        << "public:" << ln
        << "    explicit " << service.m_name
        << "Tester(QObject * parent = nullptr);" << ln << ln
        << "private Q_SLOTS:" << ln;

    for(const auto & func: service.m_functions)
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        auto funcName = capitalize(func.m_name);

        // Tests for synchronous methods

        ctx.m_out << "    void shouldExecute" << funcName << "();" << ln;

        for(const auto & e: func.m_throws)
        {
            auto exceptionTypeName = typeToStr(
                e.m_type,
                {},
                MethodType::TypeName);

            ctx.m_out << "    void shouldDeliver" << exceptionTypeName
                << "In" << funcName << "();" << ln;
        }

        ctx.m_out << "    void shouldDeliverThriftExceptionIn" << funcName
            << "();" << ln;

        // Tests for asynchronous methods

        ctx.m_out << "    void shouldExecute" << funcName << "Async();"
            << ln;

        for(const auto & e: func.m_throws)
        {
            auto exceptionTypeName = typeToStr(
                e.m_type,
                {},
                MethodType::TypeName);

            ctx.m_out << "    void shouldDeliver" << exceptionTypeName
                << "In" << funcName << "Async();" << ln;
        }

        ctx.m_out << "    void shouldDeliverThriftExceptionIn" << funcName
            << "Async();" << ln;
    }

    ctx.m_out << "};" << ln << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTestServerCpp(
    const Parser::Service & service, const QString & outPath,
    Parser & parser)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../RandomDataGenerators.h")
        << QStringLiteral("../SocketHelpers.h")
        << QStringLiteral("../../src/Future.h");

    additionalIncludes
        << (QStringLiteral("<qevercloud/services/I") + service.m_name +
            QStringLiteral(".h>"))
        << (QStringLiteral("<qevercloud/services/") + service.m_name +
            QStringLiteral("Server.h>"));

    additionalIncludes << QStringLiteral("QEventLoop")
        << QStringLiteral("<QFutureWatcher>")
        << QStringLiteral("<QTcpServer>")
        << QStringLiteral("<QtTest/QtTest>")
        << QStringLiteral("../ClearLocalFields.h");

    sortIncludes(additionalIncludes);

    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    const QString fileName = QStringLiteral("Test") + service.m_name +
        QStringLiteral(".cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Test, QStringLiteral("services"));

    writeHeaderBody(
        ctx,
        QStringLiteral("Test") + service.m_name + QStringLiteral(".h"),
        additionalIncludes,
        HeaderKind::Test, 1);

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "namespace {" << ln << ln
        << blockSeparator << ln << ln;

    ctx.m_out << "template <class T>" << ln
        << "void compareValuesWithoutLocalFields(const T & lhs, const T & rhs)"
        << ln << "{" << ln
        << indent << "T lhsCopy = lhs;" << ln
        << indent << "clearLocalFields(lhsCopy);" << ln << ln
        << indent << "T rhsCopy = rhs;" << ln
        << indent << "clearLocalFields(rhsCopy);" << ln << ln
        << indent << "Q_ASSERT(lhsCopy == rhsCopy);" << ln
        << "}" << ln << ln;

    ctx.m_out << "template <class T>" << ln
        << "void compareListValuesWithoutLocalFields("
        << "const QList<T> & lhs, const QList<T> & rhs)" << ln
        << "{" << ln
        << indent << "Q_ASSERT(lhs.size() == rhs.size());" << ln << ln
        << indent << "QList<T> lhsCopy = lhs;" << ln
        << indent << "for (auto & v: lhsCopy) {" << ln
        << indent << indent << "clearLocalFields(v);" << ln
        << indent << "}" << ln << ln
        << indent << "QList<T> rhsCopy = rhs;" << ln
        << indent << "for (auto & v: rhsCopy) {" << ln
        << indent << indent << "clearLocalFields(v);" << ln
        << indent << "}" << ln << ln
        << indent << "Q_ASSERT(lhsCopy == rhsCopy);" << ln
        << "}" << ln << ln;

    ctx.m_out << "template <class T>" << ln
        << "void compareSetValuesWithoutLocalFields("
        << "const QSet<T> & lhs, const QSet<T> & rhs)" << ln
        << "{" << ln
        << indent << "Q_ASSERT(lhs.size() == rhs.size());" << ln << ln
        << indent << "QSet<T> lhsCopy = lhs;" << ln
        << indent << "for (auto & v: lhsCopy) {" << ln
        << indent << indent << "clearLocalFields(v);" << ln
        << indent << "}" << ln << ln
        << indent << "QSet<T> rhsCopy = rhs;" << ln
        << indent << "for (auto & v: rhsCopy) {" << ln
        << indent << indent << "clearLocalFields(v);" << ln
        << indent << "}" << ln << ln
        << indent << "Q_ASSERT(lhsCopy == rhsCopy);" << ln
        << "}" << ln << ln;

    ctx.m_out << "template <class K, class V>" << ln
        << "void compareMapValuesWithoutLocalFields("
        << "const QMap<K, V> & lhs, const QMap<K, V> & rhs)" << ln
        << "{" << ln
        << indent << "Q_ASSERT(lhs.size() == rhs.size());" << ln << ln
        << indent << "QMap<K, V> lhsCopy = lhs;" << ln
        << indent << "for (auto it = lhsCopy.begin(); it != lhsCopy.end();"
        << " ++it) {" << ln
        << indent << indent << "clearLocalFields(it.value());" << ln
        << indent << "}" << ln << ln
        << indent << "QMap<K, V> rhsCopy = rhs;" << ln
        << indent << "for (auto it = rhsCopy.begin(); it != rhsCopy.end();"
        << " ++it) {" << ln
        << indent << indent << "clearLocalFields(it.value());" << ln
        << indent << "}" << ln << ln
        << indent << "Q_ASSERT(lhsCopy == rhsCopy);" << ln
        << "}" << ln << ln;

    ctx.m_out << "} // namespace" << ln << ln
        << blockSeparator << ln << ln;

    ctx.m_out << service.m_name << "Tester::" << service.m_name << "Tester"
        << "(QObject * parent) :" << ln
        << "    QObject(parent)" << ln
        << "{}" << ln << ln;

    generateTestServerHelperClassDefinition(service, ctx);

    const auto & enumerations = parser.enumerations();
    for(const auto & func: service.m_functions)
    {
        ctx.m_out << blockSeparator << ln << ln;

        auto funcName = capitalize(func.m_name);

        // Should deliver request and response for successful synchronous
        // calls

        ctx.m_out << "void " << service.m_name << "Tester::shouldExecute"
            << funcName << "()" << ln;

        ctx.m_out << "{" << ln;

        generateTestServerPrepareRequestParams(func, enumerations, ctx);
        generateTestServerPrepareRequestResponse(func, enumerations, ctx);
        generateTestServerHelperLambda(service, func, parser, ctx);
        generateTestServerSocketSetup(service, func, ctx);

        generateTestServerServiceCall(
            parser, service, func, ServiceCallKind::Sync, ctx);

        ctx.m_out << "}" << ln << ln;

        // Should deliver exceptions for synchronous calls

        for(const auto & e: func.m_throws)
        {
            auto exceptionTypeName = typeToStr(
                e.m_type,
                {},
                MethodType::TypeName);

            ctx.m_out << "void " << service.m_name << "Tester::shouldDeliver"
                << exceptionTypeName << "In" << funcName << "()" << ln;

            ctx.m_out << "{" << ln;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestExceptionResponse(parser, e, ctx);

            generateTestServerHelperLambda(
                service, func, parser, ctx, e.m_name);

            generateTestServerSocketSetup(service, func, ctx);

            generateTestServerServiceCall(
                parser, service, func, ServiceCallKind::Sync, ctx,
                exceptionTypeName, e.m_name);

            ctx.m_out << "}" << ln << ln;
        }

        // Should also properly deliver ThriftExceptions in synchronous
        // calls

        ctx.m_out << "void " << service.m_name
            << "Tester::shouldDeliverThriftExceptionIn" << funcName
            << "()" << ln;

        ctx.m_out << "{" << ln;

        Parser::Field exceptionField;
        exceptionField.m_name = QStringLiteral("thriftException");

        auto type = std::make_shared<Parser::IdentifierType>();
        type->m_identifier = QStringLiteral("ThriftException");

        exceptionField.m_type = type;

        generateTestServerPrepareRequestParams(func, enumerations, ctx);
        generateTestServerPrepareRequestExceptionResponse(
            parser, exceptionField, ctx);

        generateTestServerHelperLambda(
            service, func, parser, ctx, exceptionField.m_name);

        generateTestServerSocketSetup(service, func, ctx);

        generateTestServerServiceCall(
            parser, service, func, ServiceCallKind::Sync, ctx,
            QStringLiteral("ThriftException"), exceptionField.m_name);

        ctx.m_out << "}" << ln << ln;

        // Should deliver request and response for successful asynchonous
        // calls

        ctx.m_out << "void " << service.m_name << "Tester::shouldExecute"
            << funcName << "Async()" << ln;

        ctx.m_out << "{" << ln;

        generateTestServerPrepareRequestParams(func, enumerations, ctx);
        generateTestServerPrepareRequestResponse(func, enumerations, ctx);
        generateTestServerHelperLambda(service, func, parser, ctx);
        generateTestServerSocketSetup(service, func, ctx);

        generateTestServerServiceCall(
            parser, service, func, ServiceCallKind::Async, ctx);

        ctx.m_out << "}" << ln << ln;

        // Should deliver exceptions for asynchronous calls

        for(const auto & e: func.m_throws)
        {
            auto exceptionTypeName = typeToStr(
                e.m_type,
                {},
                MethodType::TypeName);

            ctx.m_out << "void " << service.m_name << "Tester::shouldDeliver"
                << exceptionTypeName << "In" << funcName << "Async()" << ln;

            ctx.m_out << "{" << ln;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestExceptionResponse(parser, e, ctx);

            generateTestServerHelperLambda(
                service, func, parser, ctx, e.m_name);

            generateTestServerSocketSetup(service, func, ctx);

            generateTestServerServiceCall(
                parser, service, func, ServiceCallKind::Async, ctx,
                exceptionTypeName, e.m_name);

            ctx.m_out << "}" << ln << ln;
        }

        // Should also properly deliver ThriftExceptions in synchronous
        // calls

        ctx.m_out << "void " << service.m_name
            << "Tester::shouldDeliverThriftExceptionIn" << funcName
            << "Async()" << ln;

        ctx.m_out << "{" << ln;

        generateTestServerPrepareRequestParams(func, enumerations, ctx);
        generateTestServerPrepareRequestExceptionResponse(
            parser, exceptionField, ctx);

        generateTestServerHelperLambda(
            service, func, parser, ctx, exceptionField.m_name);

        generateTestServerSocketSetup(service, func, ctx);

        generateTestServerServiceCall(
            parser, service, func, ServiceCallKind::Async, ctx,
            QStringLiteral("ThriftException"), exceptionField.m_name);

        ctx.m_out << "}" << ln << ln;
    }

    writeNamespaceEnd(ctx.m_out);

    ctx.m_out << ln
        << "#include <Test" << service.m_name << ".moc>" << ln;
}

void Generator::generateTestRandomDataGeneratorsHeader(
    Parser & parser, const QString & outPath)
{
    const auto additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/Types.h>");

    const QString fileName = QStringLiteral("RandomDataGenerators.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    // First section: generate random values of primitive types

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "QString generateRandomString(int len = 10);" << ln << ln
        << "qint8 generateRandomInt8();" << ln << ln
        << "qint16 generateRandomInt16();" << ln << ln
        << "qint32 generateRandomInt32();" << ln << ln
        << "qint64 generateRandomInt64();" << ln << ln
        << "quint8 generateRandomUint8();" << ln << ln
        << "quint16 generateRandomUint16();" << ln << ln
        << "quint32 generateRandomUint32();" << ln << ln
        << "quint64 generateRandomUint64();" << ln << ln
        << "double generateRandomDouble();" << ln << ln
        << "bool generateRandomBool();" << ln << ln;

    // Second section: generate random values of QEverCloud types

    ctx.m_out << blockSeparator << ln << ln;

    auto structsAndExceptions = parser.structures();
    structsAndExceptions << parser.exceptions();

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        ctx.m_out << "[[nodiscard]] " << s.m_name << " generateRandom"
            << s.m_name << "();" << ln << ln;
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTestRandomDataGeneratorsCpp(
    Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("<QCryptographicHash>")
        << QStringLiteral("<QDateTime>")
        << QStringLiteral("<QEventLoop>")
        << QStringLiteral("<QGlobalStatic>")
        << QStringLiteral("<QObject>")
        << QStringLiteral("<algorithm>")
        << QStringLiteral("<cstdlib>")
        << QStringLiteral("<limits>");
    sortIncludes(additionalIncludes);

    const QString fileName = QStringLiteral("RandomDataGenerators.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderBody(
        ctx,
        QStringLiteral("RandomDataGenerators.h"),
        additionalIncludes,
        HeaderKind::Test);

    // First section: auxiliary helper stuff

    ctx.m_out << "namespace {" << ln << ln
        << blockSeparator << ln << ln;

    ctx.m_out << "Q_GLOBAL_STATIC_WITH_ARGS(" << ln
        << "    QString," << ln
        << "    randomStringAvailableCharacters," << ln
        << "    (QString::fromUtf8(" << ln
        << "        \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        << "0123456789\")))" << ln << ln;

    ctx.m_out << "template <typename T>" << ln
        << "T generateRandomIntType()" << ln
        << "{" << ln
        << "    T min = std::numeric_limits<T>::min() / 4;" << ln
        << "    T max = std::numeric_limits<T>::max() / 4;" << ln
        << "    return min + (rand() % static_cast<T>(max - min + 1));"
        << ln
        << "}" << ln << ln;

    ctx.m_out << "} // namespace" << ln << ln;

    // Second section: generate random values of primitive types

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "QString generateRandomString(int len)" << ln
        << "{" << ln
        << "    if (len <= 0) {" << ln
        << "        return {};" << ln
        << "    }" << ln << ln
        << "    QString res;" << ln
        << "    res.reserve(len);" << ln
        << "    for(int i = 0; i < len; ++i) {" << ln
        << "        int index = rand() % randomStringAvailableCharacters->"
        << "length();" << ln
        << "        res.append(randomStringAvailableCharacters->at(index));"
        << ln
        << "    }" << ln << ln
        << "    return res;" << ln
        << "}" << ln << ln;

    ctx.m_out << "qint8 generateRandomInt8()" << ln
        << "{" << ln
        << "    return generateRandomIntType<qint8>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "qint16 generateRandomInt16()" << ln
        << "{" << ln
        << "    return generateRandomIntType<qint16>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "qint32 generateRandomInt32()" << ln
        << "{" << ln
        << "    return generateRandomIntType<qint32>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "qint64 generateRandomInt64()" << ln
        << "{" << ln
        << "    return generateRandomIntType<qint64>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "quint8 generateRandomUint8()" << ln
        << "{" << ln
        << "    return generateRandomIntType<quint8>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "quint16 generateRandomUint16()" << ln
        << "{" << ln
        << "    return generateRandomIntType<quint16>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "quint32 generateRandomUint32()" << ln
        << "{" << ln
        << "    return generateRandomIntType<quint32>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "quint64 generateRandomUint64()" << ln
        << "{" << ln
        << "    return generateRandomIntType<quint64>();" << ln
        << "}" << ln << ln;

    ctx.m_out << "double generateRandomDouble()" << ln
        << "{" << ln
        << "    double minval = std::numeric_limits<double>::min();" << ln
        << "    double maxval = std::numeric_limits<double>::max();" << ln
        << "    double f = (double)rand() / RAND_MAX;" << ln
        << "    return minval + f * (maxval - minval);" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool generateRandomBool()" << ln
        << "{" << ln
        << "    return generateRandomInt8() >= 0;" << ln
        << "}" << ln << ln;

    // Third section: generate random values of QEverCloud types

    ctx.m_out << blockSeparator << ln << ln;

    auto structsAndExceptions = parser.structures();
    structsAndExceptions << parser.exceptions();

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "()" << ln
            << "{" << ln;

        ctx.m_out << "    " << s.m_name << " result;" << ln;

        for(const auto & f: s.m_fields) {
            generateGetRandomValueExpression(
                f, QStringLiteral("    result."), parser, ctx.m_out);
        }

        ctx.m_out << "    return result;" << ln
            << "}" << ln << ln;
    }



    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTestClearLocalFieldsHeader(
    Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/Types.h>");

    const QString fileName = QStringLiteral("ClearLocalFields.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

    const auto relevantStructs = collectStructsWithLocalFields(parser);
    for (const auto & s: qAsConst(relevantStructs))
    {
        ctx.m_out << "void clearLocalFields(" << s.m_name << " & v);" << ln;
    }

    ctx.m_out << ln;
    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTestClearLocalFieldsCpp(
    Parser & parser, const QString & outPath)
{
    const auto additionalIncludes = QStringList()
        << QStringLiteral("<qevercloud/utility/ToRange.h>");

    const QString fileName = QStringLiteral("ClearLocalFields.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

    writeHeaderBody(
        ctx,
        QStringLiteral("ClearLocalFields.h"),
        additionalIncludes,
        HeaderKind::Test);

    const auto & structs = parser.structures();
    const auto relevantStructs = collectStructsWithLocalFields(parser);
    for (const auto & s: qAsConst(relevantStructs))
    {
        ctx.m_out << "void clearLocalFields(" << s.m_name << " & v)" << ln
            << "{" << ln;

        for (const auto & f: qAsConst(s.m_fields))
        {
            const auto * listType =
                dynamic_cast<Parser::ListType*>(f.m_type.get());

            const auto * setType =
                dynamic_cast<Parser::SetType*>(f.m_type.get());

            const auto * mapType =
                dynamic_cast<Parser::MapType*>(f.m_type.get());

            const auto * hashType =
                dynamic_cast<Parser::HashType*>(f.m_type.get());

            const auto * identifierType =
                dynamic_cast<Parser::IdentifierType*>(f.m_type.get());

            if (identifierType)
            {
                const auto typeName =
                    clearInclude(identifierType->m_identifier);

                const auto actualTypeName = aliasedTypeName(typeName);

                const auto sit = std::find_if(
                    structs.constBegin(),
                    structs.constEnd(),
                    [&actualTypeName](const Parser::Structure & strct)
                    {
                        return strct.m_name == actualTypeName;
                    });

                if ((sit != structs.constEnd()) &&
                    structContainsLocalFieldsRecursive(*sit, structs))
                {
                    if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                        ctx.m_out << indent << "if (v." << f.m_name << "()) {"
                            << ln << indent << indent
                            << "clearLocalFields(*v.mutable"
                            << capitalize(f.m_name) << "());" << ln
                            << indent << "}" << ln;
                    }
                    else {
                        ctx.m_out << indent << "clearLocalFields(v.mutable"
                            << capitalize(f.m_name) << "());" << ln;
                    }

                    continue;
                }
            }
            else if (listType)
            {
                if (const auto identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        listType->m_valueType))
                {
                    const auto actualType = aliasedTypeName(
                        clearInclude(identifierType->m_identifier));

                    const auto sit = std::find_if(
                        structs.constBegin(),
                        structs.constEnd(),
                        [&actualType](const Parser::Structure & strct)
                        {
                            return strct.m_name == actualType;
                        });

                    if ((sit != structs.constEnd()) &&
                        structContainsLocalFieldsRecursive(*sit, structs))
                    {
                        if (f.m_required ==
                            Parser::Field::RequiredFlag::Optional)
                        {
                            ctx.m_out << indent << "if (v." << f.m_name
                                << "()) {" << ln << indent << indent
                                << "for (auto & i: *v.mutable"
                                << capitalize(f.m_name) << "()) {" << ln
                                << indent << indent << indent
                                << "clearLocalFields(i);" << ln
                                << indent << indent << "}" << ln
                                << indent << "}" << ln;
                        }
                        else
                        {
                            ctx.m_out << indent << "for (auto & i: v.mutable"
                                << capitalize(f.m_name) << "()) {" << ln
                                << indent << indent << "clearLocalFields(i);"
                                << ln << indent << "}" << ln;
                        }

                        continue;
                    }
                }
            }
            else if (setType)
            {
                if (const auto identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        setType->m_valueType))
                {
                    const auto actualType = aliasedTypeName(
                        clearInclude(identifierType->m_identifier));

                    const auto sit = std::find_if(
                        structs.constBegin(),
                        structs.constEnd(),
                        [&actualType](const Parser::Structure & strct)
                        {
                            return strct.m_name == actualType;
                        });

                    if ((sit != structs.constEnd()) &&
                        structContainsLocalFieldsRecursive(*sit, structs))
                    {
                        if (f.m_required ==
                            Parser::Field::RequiredFlag::Optional)
                        {
                            ctx.m_out << indent << "if (v." << f.m_name
                                << "()) {" << ln << indent << indent
                                << "for (auto & i: *v.mutable"
                                << capitalize(f.m_name) << "()) {" << ln
                                << indent << indent << indent
                                << "clearLocalFields(i);" << ln
                                << indent << indent << "}" << ln
                                << indent << "}" << ln;
                        }
                        else
                        {
                            ctx.m_out << indent << "for (auto & i: v.mutable"
                                << capitalize(f.m_name) << "()) {" << ln
                                << indent << indent << "clearLocalFields(i);"
                                << ln << indent << "}" << ln;
                        }

                        continue;
                    }
                }
            }
            else if (mapType || hashType)
            {
                const auto & keyType =
                    (mapType ? mapType->m_keyType : hashType->m_keyType);

                const auto & valueType =
                    (mapType ? mapType->m_valueType : hashType->m_valueType);

                if (const auto identifierType =
                    std::dynamic_pointer_cast<Parser::IdentifierType>(
                        valueType))
                {
                    const auto actualType = aliasedTypeName(
                        clearInclude(identifierType->m_identifier));

                    const auto sit = std::find_if(
                        structs.constBegin(),
                        structs.constEnd(),
                        [&actualType](const Parser::Structure & strct)
                        {
                            return strct.m_name == actualType;
                        });

                    if ((sit != structs.constEnd()) &&
                        structContainsLocalFieldsRecursive(*sit, structs))
                    {
                        if (f.m_required ==
                            Parser::Field::RequiredFlag::Optional)
                        {
                            ctx.m_out << indent << "if (v." << f.m_name
                                << "()) {" << ln << indent << indent
                                << "for (auto it: toRange(*v.mutable"
                                << capitalize(f.m_name) << "()) {" << ln
                                << indent << indent << indent
                                << "clearLocalFields(it.value());" << ln
                                << indent << indent << "}" << ln
                                << indent << "}" << ln;
                        }
                        else
                        {
                            ctx.m_out << indent << "for (auto it: toRange("
                                << "v.mutable" << capitalize(f.m_name) << "()))"
                                << " {" << ln << indent << indent
                                << "clearLocalFields(it.value());" << ln
                                << indent << "}" << ln;
                        }

                        continue;
                    }
                }
            }

            if (f.m_affiliation != Parser::Field::Affiliation::Local) {
                continue;
            }

            if (f.m_required == Parser::Field::RequiredFlag::Optional)
            {
                ctx.m_out << indent << "if (v." << f.m_name << "()) {"
                    << ln << indent << indent
                    << "v." << fieldSetterName(f) << "(std::nullopt);"
                    << ln << indent << "}" << ln;

                continue;
            }

            if (mapType || hashType || listType || setType || identifierType) {
                const auto typeName = typeToStr(f.m_type, {});
                ctx.m_out << indent << "v." << fieldSetterName(f) << "("
                    << typeName << "{}" << ");" << ln;

                continue;
            }

            if (const auto primitiveType =
                    std::dynamic_pointer_cast<Parser::PrimitiveType>(f.m_type))
            {
                const auto typeName = typeToStr(primitiveType, {});
                if (typeName == QStringLiteral("bool"))
                {
                    ctx.m_out << indent << "v." << fieldSetterName(f)
                        << "(false);" << ln;
                }
                else if ((typeName == QStringLiteral("quint8")) ||
                            (typeName == QStringLiteral("qint8")) ||
                            (typeName == QStringLiteral("char")) ||
                            (typeName == QStringLiteral("quint16")) ||
                            (typeName == QStringLiteral("qint16")) ||
                            (typeName == QStringLiteral("quint32")) ||
                            (typeName == QStringLiteral("qint32")) ||
                            (typeName == QStringLiteral("quint64")) ||
                            (typeName == QStringLiteral("qint64")) ||
                            (typeName == QStringLiteral("int")) ||
                            (typeName == QStringLiteral("unsigned")))
                {
                    ctx.m_out << indent << "v." << fieldSetterName(f)
                        << "(0);" << ln;
                }
                else if ((typeName == QStringLiteral("double")) ||
                            (typeName == QStringLiteral("float")))
                {
                    ctx.m_out << indent << "v." << fieldSetterName(f)
                        << "(0.0);" << ln;
                }
            }
            else if (const auto * stringType =
                        dynamic_cast<Parser::StringType*>(f.m_type.get()))
            {
                ctx.m_out << indent << "v." << fieldSetterName(f)
                    << "(QString{});" << ln;
            }
            else if (const auto * byteArrayType =
                        dynamic_cast<Parser::ByteArrayType*>(f.m_type.get()))
            {
                ctx.m_out << indent << "v." << fieldSetterName(f)
                    << "(QByteArray{});" << ln;
            }
            else if (const auto * variantType =
                        dynamic_cast<Parser::VariantType*>(f.m_type.get()))
            {
                ctx.m_out << indent << "v." << fieldSetterName(f)
                    << "(QVariant{});" << ln;
            }
            else
            {
                ctx.m_out << indent << "v." << fieldSetterName(f)
                    << "({});" << ln;
            }
        }

        ctx.m_out << "}" << ln << ln;
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateClassAccessoryMethodsForFieldDeclarations(
    const Parser::Field & field, OutputFileContext & ctx)
{
    const QString fieldTypeName = fieldTypeToStr(field);
    const bool isPrimitiveType = isFieldOfPrimitiveType(field, fieldTypeName);

    // Const getter
    ctx.m_out << indent << "[[nodiscard]] ";

    if (isPrimitiveType) {
        ctx.m_out << fieldTypeName << " ";
    }
    else {
        ctx.m_out << "const " << fieldTypeName << " & ";
    }

    ctx.m_out << field.m_name << "() const noexcept;" << ln;

    // For non-primitive and non-string or byte array types will also provide
    // a non-const mutable value getter with name explicitly reflecting its
    // nature
    if (!isPrimitiveType &&
        !dynamic_cast<Parser::StringType*>(field.m_type.get()) &&
        !dynamic_cast<Parser::ByteArrayType*>(field.m_type.get()))
    {
        ctx.m_out << indent << "[[nodiscard]] " << fieldTypeName << " & "
            << "mutable" << capitalize(field.m_name) << "();" << ln;
    }

    // Setter
    ctx.m_out << indent << "void " << fieldSetterName(field);

    ctx.m_out << "("
        << fieldTypeName << " " << field.m_name;

    if (field.m_setterDefaultValue)
    {
        ctx.m_out << " = " << field.m_setterDefaultValue->m_value;
    }

    ctx.m_out << ");" << ln;
}

void Generator::generateClassAccessoryMethodsForFieldDefinitions(
    const Parser::Structure & s, const Parser::Field & field,
    OutputFileContext & ctx)
{
    const QString fieldTypeName = fieldTypeToStr(field);
    const bool isPrimitiveType = isFieldOfPrimitiveType(field, fieldTypeName);

    // Const getter
    if (isPrimitiveType) {
        ctx.m_out << fieldTypeName << " ";
    }
    else {
        ctx.m_out << "const " << fieldTypeName << " & ";
    }

    ctx.m_out << s.m_name << "::" << field.m_name << "() const noexcept" << ln
        << "{" << ln
        << indent << "return d->m_" << field.m_name << ";" << ln
        << "}" << ln << ln;

    // Non-const getter
    if (!isPrimitiveType &&
        !dynamic_cast<Parser::StringType*>(field.m_type.get()) &&
        !dynamic_cast<Parser::ByteArrayType*>(field.m_type.get()))
    {
        ctx.m_out << fieldTypeName << " & "
            << s.m_name << "::" << "mutable"
            << capitalize(field.m_name) << "()" << ln
            << "{" << ln
            << indent << "return d->m_" << field.m_name << ";" << ln
            << "}" << ln << ln;
    }

    // Setter
    ctx.m_out << "void " << s.m_name << "::" << fieldSetterName(field);

    ctx.m_out << "(" << fieldTypeName << " " << field.m_name << ")" << ln
        << "{" << ln
        << indent << "d->m_" << field.m_name << " = " << field.m_name << ";"
        << ln
        << "}" << ln << ln;
}

void Generator::generateServiceClassDeclaration(
    const Parser::Service & service,
    const ServiceClassType serviceClassType,
    OutputFileContext & ctx)
{
    QString className;
    if (serviceClassType == ServiceClassType::Durable) {
        className = QStringLiteral("Durable");
    }
    className += service.m_name;

    auto serviceName = decapitalize(service.m_name);

    ctx.m_out << "class Q_DECL_HIDDEN " << className
        << ": public I" << service.m_name << ln << "{" << ln;
    ctx.m_out << "    Q_DISABLE_COPY(" << className << ")" << ln;
    ctx.m_out << "public:" << ln;

    ctx.m_out << "    explicit " << className << "(" << ln;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "            QString " << serviceName << "Url = {},"
            << ln;

        if (service.m_name == QStringLiteral("NoteStore")) {
            ctx.m_out << "            std::optional<Guid> "
                << "linkedNotebookGuid = {}," << ln;
        }
    }
    else {
        ctx.m_out << "            I" << service.m_name << "Ptr service,"
            << ln;
    }

    ctx.m_out << "            IRequestContextPtr ctx = {}";

    if (serviceClassType == ServiceClassType::Durable) {
        ctx.m_out << "," << ln;
        ctx.m_out << "            "
            << "IRetryPolicyPtr retryPolicy = newRetryPolicy()";
    }

    ctx.m_out << ") :" << ln;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "        m_url(std::move(" << serviceName << "Url)),"
            << ln;

        if (service.m_name == QStringLiteral("NoteStore"))
        {
            ctx.m_out << "        m_linkedNotebookGuid(std::move("
                << "linkedNotebookGuid))," << ln;
        }
    }
    else {
        ctx.m_out << "        m_service(std::move(service))," << ln
            << "        m_durableService(newDurableService("
            << "retryPolicy, ctx))," << ln;
    }

    ctx.m_out << "        m_ctx(std::move(ctx))" << ln
        << "    {" << ln
        << "        if (!m_ctx) {" << ln
        << "            m_ctx = newRequestContext();" << ln
        << "        }" << ln
        << "    }" << ln
        << ln;

    if (serviceClassType == ServiceClassType::NonDurable)
    {
        ctx.m_out << "    " << className
            << "() :" << ln
            << "        m_ctx{newRequestContext()}" << ln
            << "    {}" << ln << ln;

        ctx.m_out << "    void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << ln
            << "    {" << ln
            << "        m_url = std::move(" << serviceName << "Url);" << ln
            << "    }"
            << ln << ln;

        ctx.m_out << "    [[nodiscard]] QString " << serviceName
            << "Url() const override" << ln
            << "    {" << ln
            << "        return m_url;" << ln
            << "    }"
            << ln << ln;

        if (service.m_name == QStringLiteral("NoteStore"))
        {
            ctx.m_out << "    [[nodiscard]] const std::optional<Guid> & "
                << "linkedNotebookGuid() const override" << ln
                << "    {" << ln
                << "        return m_linkedNotebookGuid;" << ln
                << "    }" << ln
                << ln;

            ctx.m_out << "    void setLinkedNotebookGuid("
                << "std::optional<Guid> linkedNotebookGuid) override" << ln
                << "    {" << ln
                << "        m_linkedNotebookGuid = "
                << "std::move(linkedNotebookGuid);" << ln
                << "    }" << ln
                << ln;
        }
    }
    else
    {
        ctx.m_out << "    ~" << className << "() = default;" << ln << ln;

        ctx.m_out << "    void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << ln
            << "    {" << ln
            << "        m_service->set" << service.m_name << "Url("
            << serviceName << "Url);" << ln
            << "    }"
            << ln << ln;

        ctx.m_out << "    [[nodiscard]] QString " << serviceName
            << "Url() const override" << ln
            << "    {" << ln
            << "        return m_service->" << serviceName << "Url();" << ln
            << "    }"
            << ln << ln;

        if (service.m_name == QStringLiteral("NoteStore"))
        {
            ctx.m_out << "    [[nodiscard]] const std::optional<Guid> & "
                << "linkedNotebookGuid() const override" << ln
                << "    {" << ln
                << "        return m_service->linkedNotebookGuid();" << ln
                << "    }"
                << ln << ln;

            ctx.m_out << "    void setLinkedNotebookGuid("
                << "std::optional<Guid> linkedNotebookGuid) override" << ln
                << "    {" << ln
                << "        m_service->setLinkedNotebookGuid("
                << "std::move(linkedNotebookGuid));" << ln
                << "    }"
                << ln << ln;
        }
    }

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        const auto funcReturnTypeName = typeToStr(func.m_type, func.m_name);

        ctx.m_out << "    ";
        if (funcReturnTypeName != QStringLiteral("void")) {
            ctx.m_out << "[[nodiscard]] ";
        }

        ctx.m_out << funcReturnTypeName << " "
            << func.m_name << "(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << ln;
        }

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_initializer) {
                ctx.m_out << " = " << valueToStr(
                    param.m_initializer, param.m_type,
                    func.m_name + QStringLiteral(", ") + param.m_name);
            }

            ctx.m_out << "," << ln;
        }

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") override;" << ln << ln;

        ctx.m_out << "    QFuture<" << funcReturnTypeName << "> " << func.m_name
            << "Async(" << ln;
        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_initializer) {
                ctx.m_out << " = " << valueToStr(
                    param.m_initializer,
                    param.m_type,
                    func.m_name + QStringLiteral(", ") + param.m_name);
            }

            ctx.m_out << "," << ln;
        }

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") override;" << ln << ln;
    }

    ctx.m_out << "private:" << ln;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "    QString m_url;" << ln;

        if (service.m_name == QStringLiteral("NoteStore")) {
            ctx.m_out << "    std::optional<Guid> m_linkedNotebookGuid;"
                << ln;
        }
    }
    else {
        ctx.m_out << "    I" << service.m_name << "Ptr m_service;" << ln;
        ctx.m_out << "    IDurableServicePtr m_durableService;" << ln;
    }

    ctx.m_out << "    IRequestContextPtr m_ctx;" << ln;
    ctx.m_out << "};" << ln << ln;
}

void Generator::generateServiceClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & f: service.m_functions)
    {
        ctx.m_out << blockSeparator << ln << ln;

        QString prepareParamsName = service.m_name + capitalize(f.m_name) +
            QStringLiteral("PrepareParams");

        QString readReplyName = service.m_name + capitalize(f.m_name) +
            QStringLiteral("ReadReply");

        int lastId = f.m_params.last().m_id;

        bool isVoidResult =
            (std::dynamic_pointer_cast<Parser::VoidType>(f.m_type) != nullptr);

        const auto funcReturnTypeName = (isVoidResult
                                         ? QStringLiteral("void")
                                         : typeToStr(f.m_type, f.m_name));

        ctx.m_out << "namespace {" << ln << ln;

        ctx.m_out << "QByteArray " << prepareParamsName << "(" << ln;
        for(const auto & param: f.m_params)
        {
            ctx.m_out << "    " << typeToStr(
                param.m_type,
                f.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_id != lastId) {
                ctx.m_out << "," << ln;
            }
        }

        ctx.m_out << ")" << ln;
        ctx.m_out << "{" << ln;

        auto logComponentName = camelCaseToSnakeCase(service.m_name);

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << prepareParamsName << "\");" << ln << ln;
        ctx.m_out << "    ThriftBinaryBufferWriter writer;" << ln;
        ctx.m_out << "    qint32 cseqid = 0;" << ln << ln;
        ctx.m_out << "    writer.writeMessageBegin(" << ln;
        ctx.m_out << "        QStringLiteral(\"" << f.m_name << "\")," << ln
            << "        ThriftMessageType::T_CALL," << ln
            << "        cseqid);" << ln << ln;
        ctx.m_out << "    writer.writeStructBegin(" << ln;
        ctx.m_out << "        QStringLiteral(\"" << service.m_name
            << "_" << f.m_name << "_pargs\"));" << ln;

        writeThriftWriteFields(
            ctx.m_out, f.m_params, f.m_name, QLatin1String(""));

        ctx.m_out << "    writer.writeFieldStop();" << ln;
        ctx.m_out << "    writer.writeStructEnd();" << ln;
        ctx.m_out << "    writer.writeMessageEnd();" << ln;
        ctx.m_out << "    return writer.buffer();" << ln;
        ctx.m_out << "}" << ln << ln;

        ctx.m_out << funcReturnTypeName
            << " " << readReplyName << "(QByteArray reply)" << ln;
        ctx.m_out << "{" << ln;

        if (!isVoidResult) {
            ctx.m_out << "    bool resultIsSet = false;" << ln
                << "    " << typeToStr(f.m_type, f.m_name)
                << " result = " << typeToStr(f.m_type, f.m_name)
                << "();" << ln;
        }

        ctx.m_out << "    ThriftBinaryBufferReader reader(reply);" << ln
            << "    qint32 rseqid = 0;" << ln
            << "    QString fname;" << ln
            << "    ThriftMessageType mtype;" << ln
            << "    reader.readMessageBegin(fname, mtype, rseqid);" << ln
            << "    if (mtype == ThriftMessageType::T_EXCEPTION) {" << ln
            << "        ThriftException e = readThriftException(reader);" << ln
            << "        reader.readMessageEnd();" << ln
            << "        throw e;" << ln
            << "    }" << ln
            << "    if (mtype != ThriftMessageType::T_REPLY) {" << ln
            << "        reader.skip(ThriftFieldType::T_STRUCT);" << ln
            << "        reader.readMessageEnd();" << ln
            << "        throw ThriftException(ThriftException::Type::"
            << "INVALID_MESSAGE_TYPE);" << ln
            << "    }" << ln
            << "    if (fname.compare(QStringLiteral(\"" << f.m_name
            << "\")) != 0) {" << ln
            << "        reader.skip(ThriftFieldType::T_STRUCT);" << ln
            << "        reader.readMessageEnd();" << ln
            << "        throw ThriftException(ThriftException::Type::"
            << "WRONG_METHOD_NAME);" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    ThriftFieldType fieldType;" << ln
            << "    qint16 fieldId;" << ln
            << "    reader.readStructBegin(fname);" << ln
            << "    while(true)" << ln
            << "    {" << ln
            << "        reader.readFieldBegin(fname, fieldType, fieldId);" << ln
            << "        if (fieldType == ThriftFieldType::T_STOP) {" << ln
            << "            break;" << ln
            << "        }" << ln << ln;

        if (!isVoidResult)
        {
            Parser::Field result;
            result.m_id = 0;
            result.m_name = QStringLiteral("result");
            result.m_required = Parser::Field::RequiredFlag::Required;
            result.m_type = f.m_type;

            ctx.m_out << "        if (fieldId == 0)" << ln
                << "        {" << ln
                << "            if (fieldType == "
                << typeToStr(
                    f.m_type, f.m_name, MethodType::ThriftFieldType)
                << ") {" << ln
                << "                resultIsSet = true;" << ln;

            writeThriftReadField(
                ctx.m_out, result, f.m_name + QStringLiteral("."),
                QLatin1String(""));

            ctx.m_out << "            }" << ln
                << "            else {" << ln
                << "                reader.skip(fieldType);" << ln
                << "            }" << ln
                << "        }" << ln;
        }

        bool firstThrow = isVoidResult;
        for(const auto & th: f.m_throws)
        {
            if (firstThrow) {
                firstThrow = false;
                ctx.m_out << "        ";
            }
            else {
                ctx.m_out << "        else ";
            }

            ctx.m_out << "if (fieldId == "  << th.m_id << ")" << ln
                << "        {" << ln;

            QString exceptionType = typeToStr(
                th.m_type, f.m_name + QStringLiteral(", ") + th.m_name);

            ctx.m_out << "            if (fieldType == ThriftFieldType::"
                << "T_STRUCT) {" << ln
                << "                " << exceptionType << " e;" << ln
                << "                read" << exceptionType << "(reader, e);"
                << ln;

            if (exceptionType == QStringLiteral("EDAMSystemException")) {
                ctx.m_out << "                throwEDAMSystemException(e);"
                    << ln;
            }
            else {
                ctx.m_out << "                throw e;" << ln;
            }

            ctx.m_out << "            }" << ln
                << "            else {" << ln
                << "                reader.skip(fieldType);" << ln
                << "            }" << ln
                << "        }" << ln;
        }

        ctx.m_out << "        else" << ln
            << "        {" << ln
            << "            reader.skip(fieldType);" << ln
            << "        }" << ln << ln
            << "        reader.readFieldEnd();" << ln
            << "    }" << ln << ln
            << "    reader.readStructEnd();" << ln;

        ctx.m_out << "    reader.readMessageEnd();" << ln << ln;

        if (!isVoidResult) {
            ctx.m_out << "    if (!resultIsSet) {" << ln
                << "        throw ThriftException(" << ln
                << "            ThriftException::Type::"
                << "MISSING_RESULT," << ln
                << "            QStringLiteral(\""
                << f.m_name << ": missing result\"));" << ln
                << "    }" << ln << ln
                << "    return result;" << ln;
        }

        ctx.m_out << "}" << ln << ln;

        QString asyncReadFunctionName =
            readReplyName + QStringLiteral("Async");
        ctx.m_out << "QVariant " << asyncReadFunctionName
            << "(QByteArray reply)" << ln;
        ctx.m_out << "{" << ln;
        if (isVoidResult) {
            ctx.m_out << "    " << readReplyName << "(reply);" << ln;
            ctx.m_out << "    return QVariant{};" << ln;
        }
        else {
            ctx.m_out << "    return QVariant::fromValue(" << readReplyName
                << "(reply));" << ln;
        }
        ctx.m_out << "}" << ln << ln;

        ctx.m_out << "} // namespace" << ln << ln;

        ctx.m_out << typeToStr(f.m_type, f.m_name) << " "
            << service.m_name << "::" << f.m_name << "(" << ln;
        for(const auto & param: f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type, f.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            ctx.m_out << "," << ln;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << ln
            << "{" << ln;

        ctx.m_out << "    if (!ctx) {" << ln
            << "        ctx.reset(m_ctx->clone());" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << service.m_name << "::" << f.m_name << ": request id = \""
            << ln << "        << ctx->requestId());" << ln;

        auto loggableParams = loggableFields(f.m_params);
        if (!loggableParams.isEmpty())
        {
            ctx.m_out << "    QEC_TRACE(\"" << logComponentName
                << "\", \"Parameters:\\n\"" << ln;
            auto lastLoggableParamId = loggableParams.last().m_id;
            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        << \"    " << param.m_name << " = \" << "
                    << param.m_name;
                if (param.m_id == lastLoggableParamId) {
                    ctx.m_out << ");" << ln;
                }
                else {
                    ctx.m_out << " << \"\\n\"" << ln;
                }
            }
        }

        ctx.m_out << ln;

        ctx.m_out << "    QByteArray params = " << prepareParamsName << "("
            << ln;
        for(const auto & param : f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                ctx.m_out << "        ctx->authenticationToken()";
            }
            else {
                ctx.m_out << "        " << param.m_name;
            }

            if (param.m_id != lastId) {
                ctx.m_out << "," << ln;
            }
        }
        ctx.m_out << ");" << ln << ln;

        ctx.m_out << "    QByteArray reply = askEvernote(" << ln
            << "        m_url," << ln
            << "        params," << ln
            << "        ctx->requestTimeout()," << ln
            << "        ctx->cookies());" << ln << ln;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << "received reply for request with id = \"" << ln
            << "        << ctx->requestId());" << ln;

        if (isVoidResult) {
            ctx.m_out << "    " << readReplyName << "(reply);" << ln;
        }
        else {
            ctx.m_out << "    return " << readReplyName << "(reply);"
                << ln;
        }

        ctx.m_out << "}" << ln << ln;

        ctx.m_out << "QFuture<" << funcReturnTypeName << "> " << service.m_name
            << "::" << f.m_name << "Async(" << ln;
        for(const auto & param : f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type,
                f.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;

            ctx.m_out << "," << ln;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << ln
            << "{" << ln;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << service.m_name << "::" << f.m_name << "Async\");" << ln;
        if (!loggableParams.isEmpty())
        {
            ctx.m_out << "    QEC_TRACE(\"" << logComponentName
                << "\", \"Parameters:\\n\"" << ln;
            auto lastLoggableParamId = loggableParams.last().m_id;
            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        << \"    " << param.m_name << " = \" << "
                    << param.m_name;
                if (param.m_id == lastLoggableParamId) {
                    ctx.m_out << ");" << ln;
                }
                else {
                    ctx.m_out << " << \"\\n\"" << ln;
                }
            }
        }

        ctx.m_out << ln;
        ctx.m_out << "    if (!ctx) {" << ln
            << "        ctx.reset(m_ctx->clone());" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    QByteArray params = " << prepareParamsName << "("
            << ln;
        for(const auto & param: f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                ctx.m_out << "        ctx->authenticationToken()";
            }
            else {
                ctx.m_out << "        " << param.m_name;
            }

            if (param.m_id != lastId) {
                ctx.m_out << "," << ln;
            }
        }
        ctx.m_out << ");" << ln << ln;

        ctx.m_out << "    auto variantFuture = sendRequest(" << ln
            << "        m_url," << ln
            << "        params," << ln
            << "        ctx," << ln
            << "        " << asyncReadFunctionName << ");" << ln << ln;

        ctx.m_out << "    return convertFromVariantFuture<"
            << funcReturnTypeName << ">(std::move(variantFuture));" << ln;

        ctx.m_out << "}" << ln << ln;
    }
}

void Generator::generateDurableServiceClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        // Synchronous version

        auto funcReturnTypeName = typeToStr(func.m_type, func.m_name);

        ctx.m_out << funcReturnTypeName << " "
            << "Durable" << service.m_name << "::" << func.m_name << "(" << ln;
        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type, func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            ctx.m_out << "," << ln;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << ln
            << "{" << ln;

        ctx.m_out << "    if (!ctx) {" << ln
            << "        ctx.reset(m_ctx->clone());" << ln
            << "    }" << ln << ln;

        bool isVoidResult =
            (std::dynamic_pointer_cast<Parser::VoidType>(func.m_type) != nullptr);

        ctx.m_out << "    auto call = "
            << "IDurableService::SyncServiceCall(" << ln
            << "        [&] (IRequestContextPtr ctx)" << ln
            << "        {" << ln;

        ctx.m_out << "            ";
        if (!isVoidResult) {
            ctx.m_out << "auto res = ";
        }

        ctx.m_out << "m_service->" << func.m_name << "(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << ln;
        }

        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << ln;
        }

        ctx.m_out << "                ctx);" << ln;

        ctx.m_out << "            return IDurableService::SyncResult(QVariant";
        if (!isVoidResult) {
            ctx.m_out << "::fromValue(res)";
        }
        else {
            ctx.m_out << "()";
        }
        ctx.m_out << ", {});" << ln
            << "        });" << ln << ln;

        bool requestDescriptionIsEmpty = false;
        auto loggableParams = loggableFields(func.m_params);

        if (loggableParams.isEmpty())
        {
            requestDescriptionIsEmpty = true;
        }
        else
        {
            ctx.m_out << "    QString requestDescription;" << ln
                      << "    QTextStream strm(&requestDescription);" << ln;

            ctx.m_out << "    if (logger()->shouldLog(LogLevel::Trace, "
                << "\"durable_service\")) {" << ln;

            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        strm << \"" << param.m_name << " = \" << "
                          << param.m_name << " << \"\\n\";" << ln;
            }
            ctx.m_out << "    }" << ln << ln;
        }

        ctx.m_out << "    IDurableService::SyncRequest request(" << ln
            << "        \"" << func.m_name << "\"," << ln;

        if (!requestDescriptionIsEmpty) {
            ctx.m_out << "        requestDescription," << ln;
        }
        else {
            ctx.m_out << "        {}," << ln;
        }

        ctx.m_out << "        std::move(call));" << ln << ln;

        ctx.m_out << "    auto result = m_durableService->executeSyncRequest("
            << ln
            << "        std::move(request), ctx);" << ln << ln;

        ctx.m_out << "    if (result.second) {" << ln
            << "        std::rethrow_exception(result.second);" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    return";
        if (!isVoidResult)
        {
            if (funcReturnTypeName == QStringLiteral("QString")) {
                ctx.m_out << " result.first.toString()";
            }
            else if (funcReturnTypeName == QStringLiteral("QStringList")) {
                ctx.m_out << " result.first.toStringList()";
            }
            else if (funcReturnTypeName == QStringLiteral("QByteArray")) {
                ctx.m_out << " result.first.toByteArray()";
            }
            else if (funcReturnTypeName == QStringLiteral("bool")) {
                ctx.m_out << " result.first.toBool()";
            }
            else {
                ctx.m_out << " result.first.value<" << funcReturnTypeName << ">()";
            }
        }
        ctx.m_out << ";" << ln
            << "}" << ln << ln;

        // Asynchronous version

        ctx.m_out << "QFuture<" << funcReturnTypeName << "> Durable"
            << service.m_name << "::" << func.m_name << "Async(" << ln;
        for(const auto & param : func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;

            ctx.m_out << "," << ln;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << ln
            << "{" << ln;

        ctx.m_out << "    if (!ctx) {" << ln
            << "        ctx.reset(m_ctx->clone());" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    auto call = "
            << "IDurableService::AsyncServiceCall(" << ln
            << "        [=, service=m_service] (IRequestContextPtr ctx)" << ln
            << "        {" << ln
            << "            return convertToVariantFuture<"
            << funcReturnTypeName << ">(service->" << func.m_name << "Async("
            << ln;

        for(const auto & param : func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << ln;
        }
        ctx.m_out << "                ctx));" << ln
            << "        });" << ln << ln;

        if (!requestDescriptionIsEmpty)
        {
            ctx.m_out << "    QString requestDescription;" << ln
                      << "    QTextStream strm(&requestDescription);" << ln;

            ctx.m_out << "    if (logger()->shouldLog(LogLevel::Trace, "
                << "\"durable_service\")) {" << ln;

            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        strm << \"" << param.m_name << " = \" << "
                          << param.m_name << " << \"\\n\";" << ln;
            }
            ctx.m_out << "    }" << ln << ln;
        }

        ctx.m_out << "    IDurableService::AsyncRequest request(" << ln
            << "        \"" << func.m_name << "\"," << ln;

        if (!requestDescriptionIsEmpty) {
            ctx.m_out  << "        requestDescription," << ln;
        }
        else {
            ctx.m_out << "        {}," << ln;
        }

        ctx.m_out  << "        std::move(call));" << ln << ln;

        ctx.m_out << "    return convertFromVariantFuture<"
            << funcReturnTypeName << ">(m_durableService->executeAsyncRequest("
            << ln
            << "        std::move(request), ctx));" << ln;

        ctx.m_out << "}" << ln << ln;
    }
}

void Generator::generateServerClassDeclaration(
    const Parser::Service & service, OutputFileContext & ctx)
{
    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << "/**" << ln
        << " * @brief The " << service.m_name << "Server class represents"
        << ln
        << " * customizable server for " << service.m_name << " requests."
        << ln
        << " * It is primarily used for testing of QEverCloud" << ln
        << " */" << ln;

    ctx.m_out << "class QEVERCLOUD_EXPORT " << service.m_name
        << "Server: public QObject" << ln << "{" << ln;
    ctx.m_out << "    Q_OBJECT" << ln;
    ctx.m_out << "    Q_DISABLE_COPY(" << service.m_name << "Server)" << ln;

    ctx.m_out << "public:" << ln
        << "    explicit " << service.m_name
        << "Server(QObject * parent = nullptr);" << ln << ln;

    ctx.m_out << "Q_SIGNALS:" << ln;
    ctx.m_out << "    // Signals notifying listeners about incoming requests"
        << ln;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void " << func.m_name << "Request(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << ln;
        }

        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        ";

            auto paramType = typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            if (paramType.startsWith(QStringLiteral("const "))) {
                paramType = paramType.mid(6);
            }
            if (paramType.endsWith(QStringLiteral(" &"))) {
                paramType = paramType.mid(0, paramType.size() - 2);
            }
            ctx.m_out << paramType << " " << param.m_name << "," << ln;
        }

        if (!func.m_params.isEmpty()) {
            ctx.m_out << "        ";
        }
        ctx.m_out << "IRequestContextPtr ctx);" << ln << ln;
    }

    ctx.m_out << "    // Signals used to send encoded response data" << ln;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void " << func.m_name << "RequestReady(" << ln
            << "        QByteArray data);" << ln << ln;
    }

    ctx.m_out << "public Q_SLOTS:" << ln;
    ctx.m_out << "    // Slot used to deliver requests to the server" << ln
        << "    void onRequest(QByteArray data);" << ln << ln;

    ctx.m_out << "    // Slots for replies to requests" << ln;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void on" << capitalize(func.m_name) << "RequestReady("
            << ln;
        auto responseType = typeToStr(func.m_type, func.m_name);
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        " << responseType << " value," << ln;
        }
        ctx.m_out << "        std::exception_ptr e);" << ln << ln;
    }

    ctx.m_out << "};" << ln << ln;
}

void Generator::generateServerClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    ctx.m_out << blockSeparator << ln << ln;

    ctx.m_out << service.m_name << "Server::" << service.m_name
        << "Server(QObject * parent) :" << ln
        << "    QObject(parent)" << ln
        << "{}" << ln << ln;

    ctx.m_out << "void " << service.m_name << "Server::onRequest(QByteArray data)"
        << ln
        << "{" << ln
        << "    ThriftBinaryBufferReader reader(data);" << ln
        << "    qint32 rseqid = 0;" << ln
        << "    QString fname;" << ln
        << "    ThriftMessageType mtype;" << ln
        << "    reader.readMessageBegin(fname, mtype, rseqid);" << ln << ln;

    ctx.m_out << "    if (mtype != ThriftMessageType::T_CALL) {" << ln
        << "        reader.skip(ThriftFieldType::T_STRUCT);" << ln
        << "        reader.readMessageEnd();" << ln
        << "        throw ThriftException("
        << "ThriftException::Type::INVALID_MESSAGE_TYPE);" << ln
        << "    }" << ln << ln;

    bool firstFunc = true;
    for (const auto & func: qAsConst(service.m_functions))
    {
        ctx.m_out << "    ";

        if (!firstFunc) {
            ctx.m_out << "else ";
        }

        ctx.m_out << "if (fname == QStringLiteral(\"" << func.m_name
            << "\"))" << ln
            << "    {" << ln;

        quint32 paramCount = 0;
        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            const QString paramTypeName = typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::TypeName);

            ctx.m_out << "        " << paramTypeName;
            ctx.m_out << " " << param.m_name;

            if (paramTypeName == QStringLiteral("bool"))
            {
                ctx.m_out << " = false";
            }
            else if ( (paramTypeName == QStringLiteral("quint8")) ||
                      (paramTypeName == QStringLiteral("qint8")) ||
                      (paramTypeName == QStringLiteral("char")) ||
                      (paramTypeName == QStringLiteral("quint16")) ||
                      (paramTypeName == QStringLiteral("qint16")) ||
                      (paramTypeName == QStringLiteral("quint32")) ||
                      (paramTypeName == QStringLiteral("qint32")) ||
                      (paramTypeName == QStringLiteral("quint64")) ||
                      (paramTypeName == QStringLiteral("qint64")) ||
                      (paramTypeName == QStringLiteral("int")) ||
                      (paramTypeName == QStringLiteral("unsigned")) )
            {
                ctx.m_out << " = 0";
            }
            else if ( (paramTypeName == QStringLiteral("double")) ||
                      (paramTypeName == QStringLiteral("float")) )
            {
                ctx.m_out << " = 0.0";
            }

            ctx.m_out << ";" << ln;

            ++paramCount;
        }

        ctx.m_out << "        IRequestContextPtr ctx;" << ln << ln;

        ctx.m_out << "        parse" << capitalize(service.m_name)
            << capitalize(func.m_name) << "Params(" << ln;

        ctx.m_out << "            reader," << ln;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "            " << param.m_name << "," << ln;
        }

        ctx.m_out << "            ctx);" << ln << ln;

        ctx.m_out << "        Q_EMIT " << func.m_name << "Request(" << ln;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "            " << param.m_name;
            ctx.m_out << "," << ln;
        }

        ctx.m_out << "            ctx);" << ln;
        ctx.m_out << "    }" << ln;

        firstFunc = false;
    }

    ctx.m_out << "}" << ln << ln;

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "void " << service.m_name << "Server::on"
            << capitalize(func.m_name) << "RequestReady(" << ln;

        auto responseTypeName = typeToStr(func.m_type, func.m_name);
        if (responseTypeName != QStringLiteral("void")) {
            ctx.m_out << "    " << responseTypeName << " value," << ln;
        }
        ctx.m_out << "    std::exception_ptr e)"
            << ln;

        ctx.m_out << "{" << ln;

        ctx.m_out << "    ThriftBinaryBufferWriter writer;" << ln
            << "    qint32 cseqid = 0;" << ln << ln;

        ctx.m_out << "    if (e)" << ln
            << "    {" << ln
            << "        try" << ln
            << "        {" << ln
            << "            std::rethrow_exception(e);" << ln
            << "        }" << ln
            << "        catch(const ThriftException & exception)" << ln
            << "        {" << ln
            << "            writer.writeMessageBegin(" << ln
            << "                QStringLiteral(\"" << func.m_name << "\"),"
            << ln
            << "                ThriftMessageType::T_EXCEPTION," << ln
            << "                cseqid);" << ln
            << "            writeThriftException(writer, exception);" << ln
            << "            writer.writeMessageEnd();" << ln << ln
            << "            Q_EMIT " << func.m_name << "RequestReady(" << ln
            << "                writer.buffer());" << ln
            << "            return;" << ln
            << "        }" << ln
            << "        catch(...)" << ln
            << "        {" << ln
            << "            // Will be handled below" << ln
            << "        }" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    writer.writeMessageBegin(" << ln
            << "        QStringLiteral(\"" << func.m_name << "\")," << ln
            << "        ThriftMessageType::T_REPLY," << ln
            << "        cseqid);" << ln << ln;

        ctx.m_out << "    writer.writeStructBegin(" << ln
            << "        QStringLiteral(\"" << func.m_name << "\"));" << ln << ln;

        if (!func.m_throws.isEmpty())
        {
            ctx.m_out << "    if (e)" << ln
                << "    {" << ln;

            ctx.m_out << "        try" << ln
                << "        {" << ln
                << "            std::rethrow_exception(e);" << ln
                << "        }" << ln;

            for(const auto & th: func.m_throws)
            {
                QString exceptionType = typeToStr(th.m_type, {});

                ctx.m_out << "        catch(const " << exceptionType << " & e)"
                    << ln
                    << "        {" << ln
                    << "            writer.writeFieldBegin(" << ln
                    << "                QStringLiteral(\"" << exceptionType
                    << "\")," << ln
                    << "                ThriftFieldType::T_STRUCT," << ln
                    << "                " << th.m_id << ");" << ln << ln;

                ctx.m_out << "            write" << exceptionType
                    << "(writer, e);" << ln
                    << "            writer.writeFieldEnd();" << ln << ln
                    << "            // Finalize message and return immediately"
                    << ln
                    << "            writer.writeStructEnd();" << ln
                    << "            writer.writeMessageEnd();" << ln << ln
                    << "            Q_EMIT " << func.m_name << "RequestReady("
                    << ln
                    << "                writer.buffer());" << ln
                    << "            return;" << ln
                    << "        }" << ln;
            }

            ctx.m_out << "        catch(const std::exception & e)" << ln
                << "        {" << ln
                << "            // TODO: more proper error handling" << ln
                << "            QEC_ERROR(\"server\", \"Unknown exception: \""
                << " << e.what());" << ln
                << "        }" << ln;

            ctx.m_out << "        catch(...)" << ln
                << "        {" << ln
                << "            // TODO: more proper error handling" << ln
                << "            QEC_ERROR(\"server\", \"Unknown exception\");"
                << ln
                << "        }" << ln;

            ctx.m_out << "    }" << ln << ln;
        }

        ctx.m_out << "    writer.writeFieldBegin(" << ln
            << "        QStringLiteral(\"" << func.m_name << "\")," << ln
            << "        ";

        if (responseTypeName != QStringLiteral("void")) {
            ctx.m_out << typeToStr(
                func.m_type,
                func.m_name,
                MethodType::ThriftFieldType);
        }
        else {
            ctx.m_out << "ThriftFieldType::T_VOID";
        }

        ctx.m_out << "," << ln << "        0);" << ln;

        if (responseTypeName != QStringLiteral("void"))
        {
            ctx.m_out << "    "
                << typeToStr(func.m_type, func.m_name, MethodType::WriteMethod);

            auto listType = std::dynamic_pointer_cast<Parser::ListType>(func.m_type);
            auto mapType = std::dynamic_pointer_cast<Parser::MapType>(func.m_type);
            auto setType = std::dynamic_pointer_cast<Parser::SetType>(func.m_type);

            if (listType)
            {
                ctx.m_out << typeToStr(
                    listType->m_valueType,
                    func.m_name,
                    MethodType::ThriftFieldType);
                ctx.m_out << ", value.size());" << ln;

                ctx.m_out << "    for(const auto & v: qAsConst(value)) {"
                    << ln << "        ";

                ctx.m_out << typeToStr(
                    listType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "v);" << ln
                    << "    }" << ln
                    << "    writer.writeListEnd();" << ln;
            }
            else if (setType)
            {
                ctx.m_out << typeToStr(
                    listType->m_valueType,
                    func.m_name,
                    MethodType::ThriftFieldType);
                ctx.m_out << ", value.size());" << ln;

                ctx.m_out << "    for(const auto & v: qAsConst(value)) {"
                    << ln << "        ";

                ctx.m_out << typeToStr(
                    setType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "v);" << ln
                    << "    }" << ln
                    << "    writer.writeSetEnd();" << ln;
            }
            else if (mapType)
            {
                ctx.m_out << typeToStr(
                    mapType->m_keyType,
                    func.m_name,
                    MethodType::ThriftFieldType);
                ctx.m_out << ", ";
                ctx.m_out << typeToStr(
                    mapType->m_valueType,
                    func.m_name,
                    MethodType::ThriftFieldType);
                ctx.m_out << ", value);" << ln;

                ctx.m_out << "    for(const auto & it: toRange(qAsConst(value))) {"
                    << ln << "        ";

                ctx.m_out << typeToStr(
                    mapType->m_keyType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "it.key());" << ln;

                ctx.m_out << typeToStr(
                    mapType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "it.value());" << ln;

                ctx.m_out << "    }" << ln
                    << "    writer.writeMapEnd();" << ln;
            }
            else
            {
                ctx.m_out << "value);" << ln;
            }
        }

        ctx.m_out << "    writer.writeFieldEnd();" << ln << ln;

        ctx.m_out << "    writer.writeFieldBegin(QString(), "
            << "ThriftFieldType::T_STOP, 0);" << ln
            << "    writer.writeFieldEnd();" << ln << ln;

        ctx.m_out << "    writer.writeStructEnd();" << ln
            << "    writer.writeMessageEnd();" << ln << ln;

        ctx.m_out << "    Q_EMIT " << func.m_name << "RequestReady(" << ln
            << "        writer.buffer());" << ln;

        ctx.m_out << "}" << ln << ln;
    }
}

void Generator::generateServerHelperFunctions(
    const Parser::Service & service, OutputFileContext & ctx)
{
    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    for(const auto & func: service.m_functions)
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << blockSeparator << ln << ln;

        ctx.m_out << "void parse" << capitalize(service.m_name)
            << capitalize(func.m_name) << "Params(" << ln
            << "    ThriftBinaryBufferReader & reader," << ln;

        bool hasAuthenticationToken = false;
        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                hasAuthenticationToken = true;
                continue;
            }

            auto paramType = typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            if (paramType.startsWith(QStringLiteral("const "))) {
                paramType = paramType.mid(6);
            }
            if (!paramType.endsWith(QStringLiteral(" &"))) {
                paramType += QStringLiteral(" &");
            }

            ctx.m_out << "    " << paramType << " " << param.m_name
                << "," << ln;
        }

        ctx.m_out << "    IRequestContextPtr & ctx)" << ln
            << "{" << ln;

        ctx.m_out << "    ThriftFieldType fieldType;" << ln
            << "    qint16 fieldId;" << ln;

        if (hasAuthenticationToken) {
            ctx.m_out << "    QString authenticationToken;" << ln;
        }

        ctx.m_out << ln;
        ctx.m_out << "    QString fname =" << ln
            << "        QStringLiteral(\""
            << service.m_name << "_" << func.m_name << "_pargs\");" << ln;


        ctx.m_out << ln;
        ctx.m_out << "    reader.readStructBegin(fname);" << ln
            << "    while(true)" << ln
            << "    {" << ln
            << "        reader.readFieldBegin(fname, fieldType, fieldId);"
            << ln
            << "        if (fieldType == ThriftFieldType::T_STOP) {" << ln
            << "            break;" << ln
            << "        }" << ln << ln;

        bool firstParam = true;
        for(const auto & param: func.m_params)
        {
            ctx.m_out << "        ";

            if (!firstParam) {
                ctx.m_out << "else ";
            }
            else {
                firstParam = false;
            }

            ctx.m_out << "if (fieldId == " << param.m_id << ")"
                << ln
                << "        {" << ln
                << "            if (fieldType == "
                << typeToStr(
                    param.m_type, param.m_name, MethodType::ThriftFieldType)
                << ") {" << ln;

            writeThriftReadField(
                ctx.m_out, param, param.m_name + QStringLiteral("."),
                QLatin1String(""));

            ctx.m_out << "            }" << ln
                << "            else {" << ln
                << "                reader.skip(fieldType);" << ln
                << "            }" << ln
                << "        }" << ln;
        }

        ctx.m_out << "        else" << ln
            << "        {" << ln
            << "            reader.skip(fieldType);" << ln
            << "        }" << ln << ln
            << "        reader.readFieldEnd();" << ln;

        ctx.m_out << "    }" << ln << ln
            << "    reader.readStructEnd();" << ln
            << "    reader.readMessageEnd();" << ln << ln;

        ctx.m_out << "    ctx = newRequestContext(";
        if (hasAuthenticationToken) {
            ctx.m_out << "authenticationToken";
        }
        ctx.m_out << ");" << ln << "}" << ln << ln;
    }
}

void Generator::generateSources(Parser & parser, const QString & outPath)
{
    if (parser.unions().count() > 0) {
        throw std::runtime_error("unions are not supported.");
    }

    m_baseTypes << QStringLiteral("bool") << QStringLiteral("byte")
        << QStringLiteral("i16") << QStringLiteral("i32")
        << QStringLiteral("i64") << QStringLiteral("double")
        << QStringLiteral("string") << QStringLiteral("binary");

    const auto & structures = parser.structures();
    for(const auto & s: structures) {
        m_allStructs << s.m_name;
    }

    const auto & exceptions = parser.exceptions();
    for(const auto & e: exceptions) {
        m_allExceptions << e.m_name;
    }

    const auto & enumerations = parser.enumerations();
    for(const auto & e: enumerations) {
        m_allEnums << e.m_name;
    }

    const auto & includes = parser.includes();
    for(const auto & include: includes) {
        QString s = include.m_name;
        s.replace(QStringLiteral("\""), QLatin1String(""));
        s.chop(QStringLiteral("thrift").length());
        m_includeList << s;
    }

    const auto & typeAliases = parser.typeAliases();
    for(const auto & t: typeAliases)
    {
        if (const auto * p = dynamic_cast<Parser::PrimitiveType*>(t.m_type.get()))
        {
            m_primitiveTypeAliases[t.m_name] = p->m_type;
        }
        else if (dynamic_cast<Parser::StringType*>(t.m_type.get()))
        {
            m_stringTypeAliases.insert(t.m_name);
        }
        else if (dynamic_cast<Parser::ByteArrayType*>(t.m_type.get()))
        {
            m_byteArrayTypeAliases.insert(t.m_name);
        }
    }

    generateConstantsHeader(parser, outPath);
    generateConstantsCpp(parser, outPath);

    generateErrorsHeader(parser, outPath);
    generateErrorsCpp(parser, outPath);

    generateTypesIOHeader(parser, outPath);
    generateTypesIOCpp(parser, outPath);

    generateAllExceptionsHeader(parser, outPath);
    generateExceptionsFwdHeader(parser, outPath);

    generateAllTypesHeader(parser, outPath);
    generateTypesFwdHeader(parser, outPath);

    generateTypeAliasesHeader(parser.typeAliases(), outPath);

    const QString typesSection = QStringLiteral("types");
    for (const auto & s: parser.structures())
    {
        generateTypeHeader(s, outPath, typesSection);
        generateTypeCpp(s, outPath, typesSection);
        generateTypeImplHeader(s, enumerations, outPath, typesSection);
        generateTypeImplCpp(s, outPath, typesSection);

        generateSerializationJsonHeader(s, outPath);
        generateSerializationJsonCpp(s, enumerations, outPath);
    }

    const QString exceptionsSection = QStringLiteral("exceptions");
    for (const auto & s: parser.exceptions())
    {
        generateTypeHeader(s, outPath, exceptionsSection);
        generateTypeCpp(s, outPath, exceptionsSection);
        generateTypeImplHeader(s, enumerations, outPath, exceptionsSection);
        generateTypeImplCpp(s, outPath, exceptionsSection);

        generateSerializationJsonHeader(s, outPath);
        generateSerializationJsonCpp(s, enumerations, outPath);
    }

    generateSerializationJsonTestHeader(parser, outPath);
    generateSerializationJsonTestCpp(parser, outPath);

    generateAllTypeBuildersHeader(parser, outPath);
    generateTypeBuildersFwdHeader(parser, outPath);

    for (const auto & s: parser.structures())
    {
        generateTypeBuilderHeader(s, outPath, typesSection);
        generateTypeBuilderCpp(s, enumerations, outPath, typesSection);
    }

    generateAllExceptionBuildersHeader(parser, outPath);
    generateExceptionBuildersFwdHeader(parser, outPath);

    for (const auto & s: parser.exceptions())
    {
        generateTypeBuilderHeader(s, outPath, exceptionsSection);
        generateTypeBuilderCpp(s, enumerations, outPath, exceptionsSection);
    }

    generateTypeBuildersTestHeader(parser, outPath);
    generateTypeBuildersTestCpp(parser, outPath);

    generateMetaTypesHeader(parser, outPath);
    generateMetaTypesCpp(parser, outPath);

    for (const auto & s: parser.services())
    {
        generateServiceHeader(s, outPath);
        generateServiceCpp(s, outPath);

        generateServerHeader(s, outPath);
        generateServerCpp(s, outPath);

        generateTestServerHeader(s, outPath);
        generateTestServerCpp(s, outPath, parser);
    }

    generateAllServicesHeader(parser, outPath);
    generateServicesFwdHeader(parser, outPath);

    generateFwdHeader(outPath);

    generateTestRandomDataGeneratorsHeader(parser, outPath);
    generateTestRandomDataGeneratorsCpp(parser, outPath);

    generateTestClearLocalFieldsHeader(parser, outPath);
    generateTestClearLocalFieldsCpp(parser, outPath);
}

} // namespace qevercloud_generator
