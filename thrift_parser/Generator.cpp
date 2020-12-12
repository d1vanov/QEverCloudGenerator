/**
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Sergey Skoblikov, 2015-2020 Dmitry Ivanov
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
#include "Helpers.h"

#include <QDir>
#include <QFile>
#include <QMap>
#include <QStack>
#include <QString>
#include <QTextStream>

#include <algorithm>
#include <memory>
#include <stdexcept>

using namespace qevercloud;

namespace {

////////////////////////////////////////////////////////////////////////////////

constexpr const char * ln = "\n";

constexpr const char * disclaimer =
    "/**\n"
    " * Original work: Copyright (c) 2014 Sergey Skoblikov\n"
    " * Modified work: Copyright (c) 2015-2020 Dmitry Ivanov\n"
    " *\n"
    " * This file is a part of QEverCloud project and is distributed under "
    "the terms\n"
    " * of MIT license:\n"
    " * https://opensource.org/licenses/MIT\n"
    " *\n"
    " * This file was generated from Evernote Thrift API\n"
    " */\n";

constexpr const char * blockSeparator = "/////////////////////////////////////////"
                                        "///////////////////////////////////////";

////////////////////////////////////////////////////////////////////////////////

QString generatedFileOutputPath(
    const QString & outPath, const QString & section, const OutputFileType type)
{
    QString path = outPath;
    if (type == OutputFileType::Interface) {
        path += QStringLiteral("/headers/generated");
    }
    else if (type == OutputFileType::Implementation) {
        path += QStringLiteral("/src/generated");
    }
    else if (type == OutputFileType::Test) {
        path += QStringLiteral("/src/tests/generated");
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
    m_out.setCodec("UTF-8");
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

bool Generator::shouldGenerateLocalDataMethods(const Parser::Structure & s) const
{
    if (m_allExceptions.contains(s.m_name)) {
        return false;
    }

    for (const auto & f: s.m_fields)
    {
        if (typeToStr(f.m_type) == QStringLiteral("Guid") &&
            f.m_name == QStringLiteral("guid"))
        {
            return true;
        }
    }

    return false;
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

QString Generator::capitalize(const QString & input) const
{
    if (input.isEmpty()) {
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

QString Generator::decapitalize(const QString & input) const
{
    if (input.isEmpty()) {
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

void Generator::generateGetRandomValueExpression(
    const Parser::Field & field,
    const QString & prefix,
    const Parser & parser,
    QTextStream & out,
    const QString & end)
{
    if (dynamic_cast<Parser::PrimitiveType*>(field.m_type.get()) ||
        dynamic_cast<Parser::StringType*>(field.m_type.get()) ||
        dynamic_cast<Parser::ByteArrayType*>(field.m_type.get()))
    {
        out << prefix;
        if (!field.m_name.isEmpty()) {
            out << "set" << capitalize(field.m_name) << "(";
        }

        out << getGenerateRandomValueFunction(
            typeToStr(field.m_type, {}, MethodType::TypeName));

        if (!field.m_name.isEmpty()) {
            out << ")";
        }

        out << end;
        return;
    }

    auto identifierType = std::dynamic_pointer_cast<Parser::IdentifierType>(
        field.m_type);
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
                out << "set" << capitalize(field.m_name) << "(";
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

            out << "set" << capitalize(field.m_name) << "("
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
                out << "set" << capitalize(field.m_name) << "(";
            }

            out << getGenerateRandomValueFunction(actualType);

            if (!field.m_name.isEmpty()) {
                out << ")";
            }

            out << end;
        }


        return;
    }

    auto listType = std::dynamic_pointer_cast<Parser::ListType>(field.m_type);
    if (listType)
    {
        verifyTypeIsValueOrIdentifier(listType->m_valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto valueType = typeToStr(
                listType->m_valueType,
                {},
                MethodType::TypeName);

            out << prefix << "set" << capitalize(field.m_name) << "(QList<"
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

    auto setType = std::dynamic_pointer_cast<Parser::SetType>(field.m_type);
    if (setType)
    {
        verifyTypeIsValueOrIdentifier(setType->m_valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto valueType = typeToStr(
                setType->m_valueType,
                {},
                MethodType::TypeName);

            out << prefix << "set" << capitalize(field.m_name) << "(QSet<"
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

    auto mapType = std::dynamic_pointer_cast<Parser::MapType>(field.m_type);
    if (mapType)
    {
        verifyTypeIsValueOrIdentifier(mapType->m_keyType);
        verifyTypeIsValueOrIdentifier(mapType->m_valueType);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            const auto keyType = typeToStr(
                mapType->m_keyType,
                {},
                MethodType::TypeName);

            const auto valueType = typeToStr(
                mapType->m_valueType,
                {},
                MethodType::TypeName);

            out << prefix << "set" << capitalize(field.m_name) << "(QMap<"
                << keyType << ", " << valueType << ">());" << ln;
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
            pseudoKeyField.m_type = mapType->m_keyType;
            pseudoKeyField.m_required = Parser::Field::RequiredFlag::Required;
            pseudoKeyField.m_name.clear();

            generateGetRandomValueExpression(pseudoKeyField, {}, parser, out, {});

            out << ", ";

            Parser::Field pseudoValueField;
            pseudoValueField.m_type = mapType->m_valueType;
            pseudoValueField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(pseudoValueField, {}, parser, out, QStringLiteral(");\n"));
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
    else if (typeName == QStringLiteral("QString"))
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

    constexpr const char * indent = "    ";

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
            << " " << f.m_name << " READ " << f.m_name
            << " WRITE set" << capitalize(f.m_name) << ")" << ln;
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

void Generator::writeTypeDataPrintDefinition(
    QTextStream & out, const Parser::Structure & s) const
{
    out << "void " << s.m_name
        << "::Data::print(QTextStream & strm) const" << ln
        << "{" << ln;

    constexpr const char * indent = "    ";

    out << indent << "strm << \"" << s.m_name << ": {\\n\";" << ln;

    if (shouldGenerateLocalDataMethods(s))
    {
        out << indent << indent << "strm << \"" << indent
            << "localId = \" << m_localId << \"\\n\";" << ln;

        out << indent << indent << "strm << \"" << indent
            << "parentLocalId = \" << m_parentLocalId" << " << \"\\n\";" << ln;

        out << indent << indent << "strm << \"" << indent
            << "locallyModified = \""
            << " << (m_locallyModified ? \"true\" : \"false\") << \"\\n\";"
            << ln;

        out << indent << indent << "strm << \"" << indent << "localOnly = \""
            << " << (m_localOnly ? \"true\" : \"false\") << \"\\n\";" << ln;

        out << indent << indent << "strm << \"" << indent
            << "locallyFavorited = \""
            << " << (m_locallyFavorited ? \"true\" : \"false\") << \"\\n\";"
            << ln;
    }

    bool previousOptional = false;
    for(const auto & f: s.m_fields)
    {
        auto listType = std::dynamic_pointer_cast<Parser::ListType>(f.m_type);
        auto setType = std::dynamic_pointer_cast<Parser::SetType>(f.m_type);
        auto mapType = std::dynamic_pointer_cast<Parser::MapType>(f.m_type);

        if (f.m_required == Parser::Field::RequiredFlag::Optional)
        {
            if (!previousOptional) {
                out << ln;
            }

            out << indent << "if (m_" << f.m_name << ") {" << ln
                << indent << indent << "strm << \"" << indent << f.m_name
                << " = \"" << ln;

            if (mapType)
            {
                out << indent << indent << indent << "<< \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << ln
                    << indent << indent << "for(const auto & it: toRange(*m_"
                    << f.m_name << ")) {" << ln
                    << indent << indent << indent << "strm << \""
                    << indent << indent << "[\" << it.key() << \"] = \" "
                    << "<< it.value() << \"\\n\";" << ln
                    << indent << indent << "}" << ln
                    << indent << indent << "strm << \"    }\\n\";" << ln;
            }
            else if (setType)
            {
                out << indent << indent << indent << "<< \"QSet<"
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
                out << indent << indent << indent << "<< \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << ln
                    << indent << indent << "for(const auto & v: *m_" << f.m_name
                    << ") {" << ln
                    << indent << indent << indent << "strm << \""
                    << indent << indent << "\" << v << \"\\n\";" << ln
                    << indent << indent << "}" << ln
                    << indent << indent << "strm << \"    }\\n\";" << ln;
            }
            else
            {
                out << indent << indent << indent << "<< *m_"
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

            if (mapType)
            {
                out << indent << indent << "<< \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << ln
                    << indent << "for(const auto & it: toRange(m_" << f.m_name
                    << ")) {" << ln
                    << indent << indent << "strm << \"" << indent
                    << "[\" << it.key() << \"] = \" << it.value() << \"\\n\";"
                    << ln
                    << "    }" << ln
                    << "    strm << \"}\\n\";" << ln;
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
        ctx.m_out << "        EverCloudExceptionDataPtr "
            << "exceptionData);" << ln << ln;

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
            << "EverCloudExceptionDataPtr());" << ln
            << "        }" << ln;

        ctx.m_out << "        catch(const EverCloudException & e)" << ln
            << "        {" << ln;

        ctx.m_out << "            Q_EMIT " << func.m_name << "RequestReady("
            << ln;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "                {}," << ln;
        }

        ctx.m_out << "                "
            << "e.exceptionData());" << ln
            << "        }" << ln;

        ctx.m_out << "    }" << ln << ln;

        ctx.m_out << "private:" << ln
            << "    Executor m_executor;" << ln;

        ctx.m_out << "};" << ln << ln;
    }
}

void Generator::generateTestServerAsyncValueFetcherClassDefinition(
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
            << "AsyncValueFetcher: public QObject" << ln
            << "{" << ln
            << "    Q_OBJECT" << ln
            << "public:" << ln
            << "    explicit " << service.m_name << funcName
            << "AsyncValueFetcher(QObject * parent = nullptr) :" << ln
            << "        QObject(parent)" << ln
            << "    {}" << ln << ln;

        auto responseType = typeToStr(func.m_type, {}, MethodType::TypeName);
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "    " << responseType << " m_value;" << ln;
        }

        ctx.m_out << "    EverCloudExceptionDataPtr m_exceptionData;"
            << ln << ln;

        ctx.m_out << "Q_SIGNALS:" << ln
            << "    void finished();" << ln << ln;

        ctx.m_out << "public Q_SLOTS:" << ln
            << "    void onFinished(" << ln
            << "        QVariant value," << ln
            << "        EverCloudExceptionDataPtr data," << ln
            << "        IRequestContextPtr ctx)" << ln
            << "    {" << ln;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        m_value = qvariant_cast<" << responseType
                << ">(value);" << ln;
        }
        else {
            ctx.m_out << "        Q_UNUSED(value)" << ln;
        }

        ctx.m_out << "        Q_UNUSED(ctx)" << ln;

        ctx.m_out << "        m_exceptionData = data;" << ln
            << "        Q_EMIT finished();" << ln
            << "    }" << ln
            << "};" << ln << ln;
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

        ctx.m_out << "            Q_ASSERT(" << param.m_name << " == "
            << param.m_name << "Param);" << ln;
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

    ctx.m_out << "    std::unique_ptr<I" << service.m_name << "> "
        << serviceName << "(" << ln
        << "        new" << service.m_name << "(" << ln
        << "            QStringLiteral(\"http://127.0.0.1:\") + "
        << "QString::number(port)," << ln
        << "            nullptr," << ln
        << "            nullptr," << ln
        << "            nullRetryPolicy()));" << ln;

    QString indent = QStringLiteral("    ");
    if (!exceptionTypeToCatch.isEmpty())
    {
        ctx.m_out << indent << "bool caughtException = false;" << ln;

        ctx.m_out << indent << "try" << ln
            << indent << "{" << ln;

        indent += indent;
    }

    if (callKind == ServiceCallKind::Sync)
    {
        ctx.m_out << indent;
        if (funcReturnTypeName != QStringLiteral("void")) {
            ctx.m_out << funcReturnTypeName << " res = ";
        }

        ctx.m_out << serviceName << "->" << func.m_name << "(" << ln;
    }
    else if (callKind == ServiceCallKind::Async)
    {
        ctx.m_out << indent << "AsyncResult * result = "
            << serviceName << "->" << func.m_name << "Async(" << ln;
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

        ctx.m_out << indent << "    " << param.m_name << "," << ln;
    }

    ctx.m_out << indent << "    ctx);" << ln;

    if (callKind == ServiceCallKind::Async)
    {
        auto funcName = capitalize(func.m_name);

        ctx.m_out << ln << indent << service.m_name << funcName
            << "AsyncValueFetcher valueFetcher;" << ln;

        ctx.m_out << indent << "QObject::connect(" << ln
            << indent << "    result," << ln
            << indent << "    &AsyncResult::finished," << ln
            << indent << "    &valueFetcher," << ln
            << indent << "    &"
            << service.m_name << funcName << "AsyncValueFetcher::onFinished);"
            << ln << ln;

        ctx.m_out << indent << "QEventLoop loop;" << ln
            << indent << "QObject::connect(" << ln
            << indent << "    &valueFetcher," << ln
            << indent << "    &"
            << service.m_name << funcName << "AsyncValueFetcher::finished,"
            << ln
            << indent << "    &loop," << ln
            << indent << "    &QEventLoop::quit);" << ln << ln
            << indent << "loop.exec();" << ln << ln;

        if (exceptionTypeToCatch.isEmpty())
        {
            if (funcReturnTypeName != QStringLiteral("void")) {
                ctx.m_out << indent << "QVERIFY(valueFetcher.m_value == response);"
                    << ln;
            }

            ctx.m_out << indent << "QVERIFY(valueFetcher.m_exceptionData.get() == nullptr);"
                << ln;
        }
        else
        {
            ctx.m_out << indent << "QVERIFY(valueFetcher.m_exceptionData.get() != nullptr);"
                << ln;
            ctx.m_out << indent << "valueFetcher.m_exceptionData->throwException();"
                << ln;
        }
    }

    if (!exceptionTypeToCatch.isEmpty())
    {
        if ((callKind == ServiceCallKind::Sync) &&
            (funcReturnTypeName != QStringLiteral("void")))
        {
            ctx.m_out << indent << "Q_UNUSED(res)" << ln;
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
        ctx.m_out << "    QVERIFY(res == response);" << ln;
    }
}

void Generator::writeHeaderHeader(
    OutputFileContext & ctx, const QString & fileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind,
    const QString & section)
{
    ctx.m_out << disclaimer << ln;

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());
    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;

    if (headerKind == HeaderKind::Public) {
        QStringList sectionParts = section.split(
            QChar::fromLatin1('/'),
#if QT_VERSION >= QT_VERSION_CHECK(5, 14, 0)
            Qt::SkipEmptyParts
#else
            QString::SkipEmptyParts
#endif
            );
        ctx.m_out << "#include \"../";
        for(int i = 0; i < sectionParts.size(); ++i) {
            ctx.m_out << "../";
        }
        ctx.m_out << "Export.h\"" << ln;
        ctx.m_out << ln;
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

    writeNamespaceBegin(ctx);
}

void Generator::writeHeaderBody(
    OutputFileContext & ctx, const QString & headerFileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind)
{
    ctx.m_out << disclaimer << ln;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include <generated/" << headerFileName << ">" << ln;
    }
    else {
        ctx.m_out << "#include \"" << headerFileName << "\"" << ln;
    }

    if (headerKind == HeaderKind::Test) {
        ctx.m_out << "#include \"../../Impl.h\"" << ln;
    }
    else {
        ctx.m_out << "#include \"../Impl.h\"" << ln;
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
    const QStringList & extraLinesOutsideNamespace)
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

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());

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

    const auto voidType = std::dynamic_pointer_cast<Parser::VoidType>(type);

    const auto identifierType =
        std::dynamic_pointer_cast<Parser::IdentifierType>(type);

    const auto mapType = std::dynamic_pointer_cast<Parser::MapType>(type);
    const auto setType = std::dynamic_pointer_cast<Parser::SetType>(type);
    const auto listType = std::dynamic_pointer_cast<Parser::ListType>(type);

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

    if (result.isEmpty() &&
        (methodType == MethodType::TypeName ||
         methodType == MethodType::ReadTypeName ||
         methodType == MethodType::FuncParamType ||
         methodType == MethodType::ThriftFieldType))
    {
        throw std::runtime_error(
            QString::fromUtf8("Error! unrecognized type (%1)")
            .arg(identifier).toStdString());
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

    writeHeaderHeader(ctx, fileName);

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

    auto additionalIncludes = QStringList() << QStringLiteral("<Helpers.h>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx, QStringLiteral("Constants.h"), additionalIncludes);

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

QString Generator::fieldDeclarationToStr(const Parser::Field & field)
{
    QString typeName = typeToStr(field.m_type, field.m_name);
    QString s = typeName;
    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        s = QStringLiteral("Optional<") + s + QStringLiteral(">");
    }

    s += QStringLiteral(" ") + field.m_name;
    if (field.m_initializer) {
        s += QStringLiteral(" = ") +
            valueToStr(field.m_initializer, field.m_type, field.m_name);
        return s;
    }

    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        return s;
    }

    std::optional<Parser::PrimitiveType::Type> primitiveType;
    if (const auto it = m_primitiveTypeAliases.find(typeName);
        it != m_primitiveTypeAliases.end())
    {
        primitiveType = it.value();
    }
    else if (const auto * p = dynamic_cast<Parser::PrimitiveType*>(field.m_type.get()))
    {
        primitiveType = p->m_type;
    }

    if (!primitiveType) {
        return s;
    }

    switch(*primitiveType)
    {
    case Parser::PrimitiveType::Type::Bool:
        s += QStringLiteral(" = false");
        break;
    case Parser::PrimitiveType::Type::Byte:
    case Parser::PrimitiveType::Type::Int16:
    case Parser::PrimitiveType::Type::Int32:
    case Parser::PrimitiveType::Type::Int64:
        s += QStringLiteral(" = 0");
        break;
    case Parser::PrimitiveType::Type::Double:
        s += QStringLiteral(" = 0.0");
        break;
    }

    return s;
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
        QString indent = QLatin1String("");

        const bool isOptional =
            (field.m_required == Parser::Field::RequiredFlag::Optional);

        const QString fieldSuffix =
            (fieldPrefix.isEmpty() ? QLatin1String("") : QStringLiteral("()"));

        if (isOptional) {
            indent = QStringLiteral("    ");
            out << "    if (" << fieldPrefix
                << field.m_name << fieldSuffix << ") {" << ln;
        }

        out << indent << "    writer.writeFieldBegin(" << ln
            << indent << "        QStringLiteral(\""
            << field.m_name << "\")," << ln
            << indent << "        " << typeToStr(
                field.m_type, indentPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << "," << ln
            << indent << "        " << field.m_id << ");" << ln << ln;

        QString writeMethod = typeToStr(
            field.m_type, indentPrefix + QStringLiteral(",") + field.m_name,
            MethodType::WriteMethod);

        if (writeMethod.contains(QStringLiteral("writeListBegin")))
        {
            auto valueType = std::dynamic_pointer_cast<Parser::ListType>(
                field.m_type)->m_valueType;

            out << indent << "    writer.writeListBegin("
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "length());" << ln;

            out << indent
                << "    for(const auto & value: qAsConst("
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indent << "        " << writeMethod << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indent << "    }" << ln;
            out << indent << "    writer.writeListEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            auto valueType = std::dynamic_pointer_cast<Parser::SetType>(
                field.m_type)->m_valueType;

            out << indent << "    writer.writeSetBegin("
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "count());" << ln;

            out << indent << "    for(const auto & value: qAsConst("
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indent << "        " << writeMethod
                << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indent << "    }" << ln;
            out << indent << "    writer.writeSetEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            auto keyType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_keyType;

            auto valueType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_valueType;

            out << indent << "    writer.writeMapBegin("
                << typeToStr(
                    keyType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", "
                << typeToStr(
                    valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldPrefix << field.m_name << "()"
                << (isOptional ? "->" : ".") << "size());" << ln;

            out << indent << "    for(const auto & it: "
                << "toRange(" << (isOptional ? "*" : "") << fieldPrefix
                << field.m_name << fieldSuffix << ")) {" << ln;

            QString keyWriteMethod = typeToStr(
                keyType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            QString valueWriteMethod = typeToStr(
                valueType, indentPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << indent << "        " << keyWriteMethod
                << "it.key()"
                << (keyWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indent << "        " << valueWriteMethod << "it.value()"
                << (valueWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << indent << "    }" << ln;
            out << indent << "    writer.writeMapEnd();" << ln << ln;
        }
        else
        {
            out << indent << "    " << writeMethod
                << (isOptional ? "*" : "") << fieldPrefix << field.m_name
                << fieldSuffix
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;
        }

        out << indent << "    writer.writeFieldEnd();" << ln;
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
    constexpr const char * indent = "                ";

    out << indent
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

        out << indent << "qint32 size;" << ln;
        out << indent << "ThriftFieldType elemType;" << ln;
        out << indent << "reader.readListBegin(elemType, size);" << ln;
        out << indent << "v.reserve(size);" << ln;
        out << indent << "if (elemType != " << valueThriftType
            << ") {" << ln << indent << "    throw ThriftException("
            << ln << indent << "        ThriftException::Type::"
            << "INVALID_DATA," << ln << indent
            << "        QStringLiteral(\"Incorrect list type ("
            << indentPrefix + field.m_name << ")\"));" << ln
            << indent << "}" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {"
            << ln;
        out << indent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << ln;
        out << indent << "    " << valueReadMethod << "elem);" << ln;
        out << indent << "    v.append(elem);" << ln;
        out << indent << "}" << ln;
        out << indent << "reader.readListEnd();" << ln;
    }
    else if (readMethod.contains(QStringLiteral("readSetBegin")))
    {
        auto valueType =
            std::dynamic_pointer_cast<Parser::SetType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, indentPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << ln;
        out << indent << "ThriftFieldType elemType;" << ln;
        out << indent << "reader.readSetBegin(elemType, size);" << ln;
        out << indent << "v.reserve(size);" << ln;
        out << indent << "if (elemType != " << valueThriftType
            << ") {" << ln
            << indent << "    throw ThriftException(" << ln
            << indent << "        ThriftException::Type::INVALID_DATA," << ln
            << indent << "        QStringLiteral(\"Incorrect set type ("
            << indentPrefix + field.m_name << ")\"));" << ln
            << indent << "}" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << indent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << ln;
        out << indent << "    " << valueReadMethod << "elem);" << ln;
        out << indent << "    v.insert(elem);" << ln;
        out << indent << "}" << ln;
        out << indent << "reader.readSetEnd();" << ln;
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

        out << indent << "qint32 size;" << ln;
        out << indent << "ThriftFieldType keyType;" << ln;
        out << indent << "ThriftFieldType elemType;" << ln;
        out << indent << "reader.readMapBegin(keyType, elemType, size);" << ln;
        out << indent << "if (keyType != " << keyThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map key type ("
            << indentPrefix << field.m_name << ")\"));" << ln;
        out << indent << "if (elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map value type ("
            << indentPrefix + field.m_name << ")\"));" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << indent << "    "
            << typeToStr(
                keyType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " key;" << ln;
        out << indent << "    " << keyReadMethod << "key);" << ln;
        out << indent << "    "
            << typeToStr(
                valueType, indentPrefix + field.m_name, MethodType::ReadTypeName)
            << " value;" << ln;
        out << indent << "    " << valueReadMethod << "value);" << ln;
        out << indent << "    v[key] = value;" << ln;
        out << indent << "}" << ln;
        out << indent << "reader.readMapEnd();" << ln;
    }
    else
    {
        out << indent << readMethod << "v);" << ln;
    }

    if (fieldParent.isEmpty()) {
        out << indent << field.m_name << " = ";
    }
    else {
        out << indent << fieldParent << "set" << capitalize(field.m_name)
            << "(";
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
        << QStringLiteral("../Helpers.h")
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
    extraLinesOutsideNamespace.reserve(enumerations.size());
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
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList()
        << QStringLiteral("<generated/Types.h>") << QStringLiteral("../Impl.h");
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

void Generator::generateTypesHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    ctx.m_out << disclaimer << ln;
    ctx.m_out << "#ifndef QEVERCLOUD_GENERATED_TYPES_H" << ln;
    ctx.m_out << "#define QEVERCLOUD_GENERATED_TYPES_H" << ln << ln;

    const auto & structures = parser.structures();
    QStringList typesIncludes;
    typesIncludes.reserve(structures.size());
    for (const auto & s: structures) {
        typesIncludes << s.m_name;
    }

    std::sort(typesIncludes.begin(), typesIncludes.end());

    for (const auto & include: qAsConst(typesIncludes)) {
        ctx.m_out << "#include \"types/" << include << ".h\"" << ln;
    }

    ctx.m_out << ln;

    const auto & exceptions = parser.exceptions();
    QStringList exceptionIncludes;
    exceptionIncludes.reserve(exceptions.size());
    for (const auto & s: exceptions) {
        exceptionIncludes << s.m_name;
    }

    std::sort(exceptionIncludes.begin(), exceptionIncludes.end());

    for (const auto & include: qAsConst(exceptionIncludes)) {
        ctx.m_out << "#include \"exceptions/" << include << ".h\"" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << "#endif // QEVERCLOUD_GENERATED_TYPES_H" << ln;
}

void Generator::generateTypesCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList()
        << QStringLiteral("Types_io.h") << QStringLiteral("<Helpers.h>")
        << QStringLiteral("<QUuid>") << QStringLiteral("<QDebug>")
        << QStringLiteral("<optional>");

    sortIncludes(additionalIncludes);
    writeHeaderBody(ctx, QStringLiteral("Types.h"), additionalIncludes);

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
        ctx, fileName, additionalIncludes, HeaderKind::Public, fileSection);

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
        << QStringLiteral("../../EverCloudException.h")
        << QStringLiteral("../../Printable.h")
        << QStringLiteral("../EDAMErrorCode.h");

    const bool isTypesSection = (fileSection == QStringLiteral("types"));

    const bool isExceptionsSection =
        (!isTypesSection && (fileSection == QStringLiteral("exceptions")));

    if (isTypesSection) {
        additionalIncludes << QStringLiteral("TypeAliases.h");
    }
    else if (isExceptionsSection) {
        additionalIncludes << QStringLiteral("../types/TypeAliases.h");
    }

    additionalIncludes << additionalIncludesForFields(s);

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
                QStringLiteral("../exceptions/") + dep + QStringLiteral(".h"));
            continue;
        }

        if (!isExceptionType && isExceptionsSection) {
            additionalIncludes.append(
                QStringLiteral("../types/") + dep + QStringLiteral(".h"));
            continue;
        }

        additionalIncludes.append(dep + QStringLiteral(".h"));
    }

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Public, fileSection);

    const QString indent = QStringLiteral("    ");

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

    const bool generateLocalData = shouldGenerateLocalDataMethods(s);
    if (generateLocalData) {
        generateTypeLocalDataAccessoryMethodDeclarations(s.m_name, ctx, indent);
    }

    for(const auto & f: qAsConst(s.m_fields))
    {
        if (const auto cit = s.m_fieldComments.constFind(f.m_name);
            cit != s.m_fieldComments.constEnd())
        {
            const auto lines = cit.value().split(QChar::fromLatin1('\n'));
            for(const auto & line: qAsConst(lines)) {
                ctx.m_out << indent << line << ln;
            }
        }

        generateClassAccessoryMethodsForFieldDeclarations(f, ctx, indent);
        ctx.m_out << ln;
    }

    ctx.m_out << indent
        << "void print(QTextStream & strm) const override;" << ln;

    if (m_allExceptions.contains(s.m_name)) {
        ctx.m_out << indent
            << "[[nodiscard]] const char * what() const noexcept override;"
            << ln;

        ctx.m_out << indent
            << "[[nodiscard]] EverCloudExceptionDataPtr exceptionData() const "
            << "override;" << ln;
    }

    ctx.m_out << ln;
    ctx.m_out << indent << "[[nodiscard]] bool operator==(const " << s.m_name
        << " & other) const noexcept;" << ln;

    ctx.m_out << indent << "[[nodiscard]] bool operator!=(const " << s.m_name
        << " & other) const noexcept;" << ln;

    ctx.m_out << ln;

    if (generateLocalData)
    {
        ctx.m_out << indent
            << "Q_PROPERTY(QString localId READ localId WRITE setLocalId)"
            << ln << indent
            << "Q_PROPERTY(QString parentLocalId READ parentLocalId "
            << "WRITE setParentLocalId)" << ln
            << indent
            << "Q_PROPERTY(bool locallyModified READ isLocallyModified "
            << "WRITE setLocallyModified)" << ln
            << indent
            << "Q_PROPERTY(bool localOnly READ isLocalOnly "
            << "WRITE setLocalOnly)" << ln
            << indent
            << "Q_PROPERTY(bool favorited READ isLocallyFavorited "
            << "WRITE setLocallyFavorited)" << ln;
    }

    writeTypeProperties(s, ctx);

    ctx.m_out << ln;
    ctx.m_out << "private:" << ln
        << indent << "class Impl;" << ln
        << indent << "QSharedDataPointer<Impl> d;" << ln
        << "};" << ln << ln;

    const bool isException = m_allExceptions.contains(s.m_name);
    if (isException) {
        generateExceptionDataClassDeclaration(s, ctx);
    }

    QStringList extraLinesOutsideNamespace;
    extraLinesOutsideNamespace.reserve(isException ? 2 : 1);

    QString metatypeDeclarationLine;
    QTextStream lineOut(&metatypeDeclarationLine);
    lineOut << "Q_DECLARE_METATYPE(qevercloud::" << s.m_name << ")";
    extraLinesOutsideNamespace << metatypeDeclarationLine;

    if (isException) {
        extraLinesOutsideNamespace <<
            (QStringLiteral("Q_DECLARE_METATYPE(qevercloud::") + s.m_name +
             QStringLiteral("Data)"));
    }

    writeHeaderFooter(ctx.m_out, fileName, {}, extraLinesOutsideNamespace);
}

void Generator::generateTypeCpp(
    const Parser::Structure & s, const QString & outPath,
    const QString & fileSection)
{
    const QString fileName = s.m_name + QStringLiteral(".cpp");

    OutputFileContext ctx(
        fileName, outPath, OutputFileType::Implementation, fileSection);

    ctx.m_out << disclaimer << ln;

    ctx.m_out  << "#include <generated/" << fileSection << "/" << s.m_name
        << ".h>" << ln << ln;

    ctx.m_out  << "#include \"impl/" << s.m_name << "Impl.h\"" << ln << ln;

    writeNamespaceBegin(ctx);

    constexpr const char * indent = "    ";

    ctx.m_out << s.m_name << "::" << s.m_name << "() : d(new " << s.m_name
        << "::Impl) {}" << ln << ln;

    ctx.m_out << s.m_name << "::" << s.m_name << "(const " << s.m_name
        << " & other) : d(other.d) {}" << ln << ln;

    ctx.m_out << s.m_name << "::" << s.m_name << "(" << s.m_name
        << " && other) noexcept : d(std::move(other.d))" << ln
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

    if (shouldGenerateLocalDataMethods(s)) {
        generateTypeLocalDataAccessoryMethodDefinitions(s.m_name, ctx);
    }

    for(const auto & f: qAsConst(s.m_fields)) {
        generateClassAccessoryMethodsForFieldDefinitions(s, f, ctx);
    }

    ctx.m_out << "void " << s.m_name
        << "::print(QTextStream & strm) const" << ln
        << "{" << ln
        << indent << "d->print(strm);" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool " << s.m_name << "::operator==(const " << s.m_name
        << " & other) const noexcept" << ln
        << "{" << ln
        << indent << "return *d == *other.d;" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool " << s.m_name << "::operator!=(const " << s.m_name
        << " & other) const noexcept" << ln
        << "{" << ln
        << indent << "return !(*this == other);" << ln
        << "}" << ln << ln;

    if (m_allExceptions.contains(s.m_name))
    {
        // TODO: should check if m_error is empty and if so, put
        // exception details there
        ctx.m_out << "const char * " << s.m_name << "::what() const noexcept"
            << ln << "{" << ln
            << indent << "return EvernoteException::what();" << ln
            << "}" << ln << ln;

        ctx.m_out << "EverCloudExceptionDataPtr " << s.m_name
            << "::exceptionData() const" << ln
            << "{" << ln
            << indent << "return std::make_shared<" << s.m_name << "Data>("
            << ln;

        for (const auto & f: s.m_fields)
        {
            ctx.m_out << indent << indent << f.m_name << "()";

            if (&f != &(s.m_fields.back())) {
                ctx.m_out << "," << ln;
            }
            else {
                ctx.m_out << ");" << ln;
            }
        }

        ctx.m_out << "}" << ln << ln;

        ctx.m_out << s.m_name << "Data::" << s.m_name << "Data(";

        if (s.m_fields.isEmpty())
        {
            ctx.m_out << "QString error) :" << ln
                << indent << "EvernoteExceptionData(error)," << ln
                << indent << "d(new " << s.m_name << "Data::Impl)" << ln
                << "{}" << ln;
        }
        else
        {
            ctx.m_out << ln;
            ctx.m_out << indent << "QString error," << ln;

            for (const auto & f: s.m_fields)
            {
                ctx.m_out << indent;

                if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                    ctx.m_out << "std::optional<";
                }

                ctx.m_out << typeToStr(f.m_type);

                if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                    ctx.m_out << ">";
                }

                ctx.m_out << " " << f.m_name;

                if (&f != &(s.m_fields.back())) {
                    ctx.m_out << "," << ln;
                }
                else {
                    ctx.m_out << ")";
                }
            }

            ctx.m_out << ":" << ln
                << indent << "EvernoteExceptionData(error)," << ln
                << indent << "d(new " << s.m_name << "Data::Impl)" << ln
                << "{" << ln;

            for (const auto & f: s.m_fields)
            {
                ctx.m_out << indent << "d->m_" << f.m_name << " = std::move("
                    << f.m_name << ");" << ln;
            }

            ctx.m_out << "}" << ln << ln;

            Parser::Structure sData = s;
            sData.m_name = s.m_name + QStringLiteral("Data");

            for (const auto & f: s.m_fields)
            {
                generateClassAccessoryMethodsForFieldDefinitions(
                    sData, f, ctx);
            }
        }

        ctx.m_out << s.m_name << "Data::~" << s.m_name << "Data() noexcept {}"
            << ln << ln;

        ctx.m_out << "void " << s.m_name << "Data::throwException() const" << ln
            << "{" << ln
            << indent << s.m_name << " e;" << ln;

        for (const auto & f: s.m_fields)
        {
            ctx.m_out << indent << "e.set" << capitalize(f.m_name)
                << "(d->m_" << f.m_name << ");" << ln;
        }

        ctx.m_out << indent << "throw e;" << ln
            << "}" << ln << ln;
    }

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
        QStringLiteral("<generated/") + fileSection + QStringLiteral("/") +
        s.m_name + QStringLiteral(".h>");

    QStringList additionalIncludes = QStringList()
        << publicHeaderInclude << QStringLiteral("<QSharedData>");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Private, fileSection);

    constexpr const char * indent = "    ";

    ctx.m_out << "class Q_DECL_HIDDEN " << s.m_name << "::"
        << "Impl final:" << ln
        << indent << "public QSharedData," << ln
        << indent << "public Printable" << ln
        << "{" << ln
        << "public:" << ln;

    ctx.m_out << indent << "Impl() = default;" << ln
        << indent << "Impl(const " << s.m_name
        << "::Impl & other) = default;" << ln
        << indent << "Impl(" << s.m_name
        << "::Impl && other) noexcept = default;" << ln << ln;

    ctx.m_out << indent << s.m_name << "::Impl & operator=(const "
        << s.m_name << "::Impl & other) = delete;" << ln
        << indent << s.m_name << "::Impl & operator=(" << s.m_name
        << "::Impl && other) = delete;" << ln << ln;

    ctx.m_out << indent << "~Impl() noexcept override = default;" << ln;

    ctx.m_out << ln;
    ctx.m_out << indent << "[[nodiscard]] bool operator==(const " << s.m_name
        << "::Impl & other) const noexcept;" << ln;

    ctx.m_out << indent << "[[nodiscard]] bool operator!=(const " << s.m_name
        << "::Impl & other) const noexcept;" << ln;

    ctx.m_out << ln
        << indent << "void print(QTextStream & strm) const override;" << ln
        << ln;

    if (shouldGenerateLocalDataMethods(s)) {
        ctx.m_out << indent << "QString m_localId;" << ln
            << indent << "QString m_parentLocalId;" << ln
            << indent << "bool m_locallyModified = false;" << ln
            << indent << "bool m_localOnly = false;" << ln
            << indent << "bool m_locallyFavorited = false;" << ln << ln;
    }

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

    if (m_allExceptions.contains(s.m_name))
    {
        ctx.m_out << "class Q_DECL_HIDDEN " << s.m_name
            << "Data::Impl final:" << ln
            << indent << "public QSharedData" << ln
            << "{" << ln
            << "public:" << ln;

        ctx.m_out << indent << "Impl() = default;" << ln
            << indent << "Impl(const " << s.m_name << "Data::Impl & other)"
            << " = default;" << ln
            << indent << "Impl(" << s.m_name << "Data::Impl && other) noexcept"
            << " = default;" << ln << ln;

        if (!s.m_fields.isEmpty())
        {
            ctx.m_out << indent << "explicit Impl(" << ln;

            for (const auto & f: s.m_fields)
            {
                ctx.m_out << indent << indent;

                if (f.m_required == Parser::Field::RequiredFlag::Optional)
                {
                    ctx.m_out << "std::optional<";
                }

                ctx.m_out << typeToStr(f.m_type);

                if (f.m_required == Parser::Field::RequiredFlag::Optional)
                {
                    ctx.m_out << ">";
                }

                ctx.m_out << " " << f.m_name;

                if (&f != &(s.m_fields.back())) {
                    ctx.m_out << "," << ln;
                }
                else {
                    ctx.m_out << ") :" << ln;
                }
            }

            for (const auto & f: s.m_fields)
            {
                ctx.m_out << indent << indent << "m_" << f.m_name
                    << "(std::move(" << f.m_name << "))";

                if (&f != &(s.m_fields.back())) {
                    ctx.m_out << "," << ln;
                }
                else {
                    ctx.m_out << ln;
                }
            }

            ctx.m_out << indent << "{}" << ln << ln;
        }

        ctx.m_out << indent << s.m_name << "Data::Impl & operator=(const "
            << s.m_name << "Data::Impl & other) = delete;" << ln
            << indent << s.m_name << "Data::Impl & operator=("
            << s.m_name << "Data::Impl && other) = delete;" << ln << ln;

        ctx.m_out << indent << "~Impl() noexcept = default;" << ln
            << ln;

        for (const auto & f: s.m_fields)
        {
            ctx.m_out << indent;

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "std::optional<";
            }

            ctx.m_out << typeToStr(f.m_type);

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << ">";
            }

            ctx.m_out << " m_" << f.m_name << ";" << ln;
        }

        ctx.m_out << "};" << ln << ln;
    }

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
    ctx.m_out << "#include \"" << s.m_name << "Data.h\"" << ln << ln;
    ctx.m_out << "#include <QTextStream>" << ln << ln;

    writeNamespaceBegin(ctx);

    constexpr const char * indent = "    ";

    ctx.m_out << "bool " << s.m_name
        << "::Impl::operator==(" << ln << indent << "const " << s.m_name
        << "::Impl & other) const noexcept" << ln
        << "{" << ln;

    ctx.m_out << indent << "if (this == &other) {" << ln
        << indent << indent << "return true;" << ln
        << indent << "}" << ln << ln;

    ctx.m_out << indent << "return" << ln;

    if (shouldGenerateLocalDataMethods(s)) {
        ctx.m_out << indent << indent << "m_localId == other.m_localId &&" << ln
            << indent << indent
            << "m_parentLocalId == other.m_parentLocalId &&" << ln
            << indent << indent
            << "m_locallyModified == other.m_locallyModified &&" << ln
            << indent << indent << "m_localOnly == other.m_localOnly &&" << ln
            << indent << indent
            << "m_locallyFavorited == other.m_locallyFavorited &&" << ln;
    }

    for(const auto & f: qAsConst(s.m_fields))
    {
        ctx.m_out << indent << indent << "m_" << f.m_name << " == other.m_"
            << f.m_name;

        if (&f != &(s.m_fields.constLast())) {
            ctx.m_out << " &&" << ln;
        }
        else {
            ctx.m_out << ";" << ln;
        }
    }

    ctx.m_out << "}" << ln << ln;

    ctx.m_out << "bool " << s.m_name
        << "::Impl::operator!=(" << ln << indent << "const " << s.m_name
        << "::Impl & other) const noexcept" << ln
        << "{" << ln
        << indent << "return !(*this == other);" << ln
        << "}" << ln << ln;

    writeTypeDataPrintDefinition(ctx.m_out, s);
    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateExceptionDataClassDeclaration(
    const Parser::Structure & s, OutputFileContext & ctx)
{
    const QString indent = QStringLiteral("    ");

    ctx.m_out << "/**" << ln
        << " * Asynchronous API counterpart of " << s.m_name
        << ". See EverCloudExceptionData" << ln
        << " * for more details." << ln
        << " */" << ln;

    ctx.m_out << "class QEVERCLOUD_EXPORT " << s.m_name
        << "Data: public EvernoteExceptionData" << ln
        << "{" << ln
        << indent << "Q_OBJECT" << ln
        << indent << "Q_DISABLE_COPY(" << s.m_name << "Data)" << ln
        << "public:" << ln;

    ctx.m_out << indent << "explicit " << s.m_name << "Data(";

    if (s.m_fields.isEmpty())
    {
        ctx.m_out << "QString error);" << ln;
    }
    else
    {
        ctx.m_out << ln;
        ctx.m_out << indent << indent << "QString error," << ln;

        for (const auto & f: s.m_fields)
        {
            ctx.m_out << indent << indent;

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "std::optional<";
            }

            ctx.m_out << typeToStr(f.m_type);

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << ">";
            }

            ctx.m_out << " " << f.m_name;

            if (&f != &(s.m_fields.back())) {
                ctx.m_out << "," << ln;
            }
            else {
                ctx.m_out << ");" << ln;
            }
        }
    }

    ctx.m_out << ln;

    ctx.m_out << indent << "~" << s.m_name << "Data() noexcept override;" << ln
        << ln;

    for (const auto & f: s.m_fields) {
        generateClassAccessoryMethodsForFieldDeclarations(f, ctx, indent);
        ctx.m_out << ln;
    }

    ctx.m_out << indent << "virtual void throwException() const override;"
        << ln << ln;

    ctx.m_out << "private:" << ln
        << indent << "class Impl;" << ln
        << indent << "QSharedDataPointer<Impl> d;" << ln;

    ctx.m_out << "};" << ln << ln;
}

void Generator::generateServicesHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Services.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("../AsyncResult.h")
        << QStringLiteral("../DurableService.h")
        << QStringLiteral("../RequestContext.h")
        << QStringLiteral("Constants.h")
        << QStringLiteral("Types.h")
        << QStringLiteral("<QObject>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    const auto & services = parser.services();
    for(const auto & s: services)
    {
        if (!s.m_extends.isEmpty()) {
            throw std::runtime_error("extending services is not supported");
        }

        ctx.m_out << blockSeparator << ln << ln;

        if (!s.m_docComment.isEmpty()) {
            ctx.m_out << s.m_docComment << ln;
        }

        ctx.m_out << "class QEVERCLOUD_EXPORT I" << s.m_name
            << ": public QObject" << ln << "{" << ln;
        ctx.m_out << "    Q_OBJECT" << ln;
        ctx.m_out << "    Q_DISABLE_COPY(I" << s.m_name << ")" << ln;
        ctx.m_out << "protected:"<< ln;
        ctx.m_out << "    I" << s.m_name << "(QObject * parent) :" << ln;
        ctx.m_out << "        QObject(parent)" << ln;
        ctx.m_out << "    {}" << ln << ln;
        ctx.m_out << "public:" << ln;
        ctx.m_out << "    virtual QString " << decapitalize(s.m_name)
            << "Url() const = 0;" << ln;
        ctx.m_out << "    virtual void set" << s.m_name << "Url(QString url) = 0;"
            << ln << ln;

        for(const auto & func: qAsConst(s.m_functions))
        {
            if (func.m_isOneway) {
                throw std::runtime_error("oneway functions are not supported");
            }

            if (!func.m_docComment.isEmpty())
            {
                QStringList lines = func.m_docComment.split(
                    QChar::fromLatin1('\n'));

                for(const auto & line: qAsConst(lines)) {
                    ctx.m_out << "    " << line << ln;
                }
            }

            ctx.m_out << "    virtual " << typeToStr(func.m_type, func.m_name) << " "
                 << func.m_name << "(";
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
            ctx.m_out << "    virtual AsyncResult * " << func.m_name << "Async(" << ln;
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
        ctx.m_out << "using I" << s.m_name << "Ptr = std::shared_ptr<I"
            << s.m_name << ">;" << ln << ln;
    }

    if (!services.isEmpty())
    {
        ctx.m_out << blockSeparator << ln << ln;

        for(const auto & s: services)
        {
            ctx.m_out << "QEVERCLOUD_EXPORT I" << s.m_name << " * new"
                << s.m_name << "(" << ln;

            ctx.m_out << "    QString " << decapitalize(s.m_name)
                << "Url = {}," << ln
                << "    IRequestContextPtr ctx = {}," << ln
                << "    QObject * parent = nullptr," << ln
                << "    IRetryPolicyPtr retryPolicy = {});" << ln << ln;
        }
    }

    QStringList metatypeDeclarations;
    metatypeDeclarations.reserve(6);
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::Notebook>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::Tag>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::SavedSearch>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::NoteVersionId>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::SharedNotebook>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::LinkedNotebook>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::BusinessInvitation>)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList<qevercloud::UserProfile>)");

    writeHeaderFooter(ctx.m_out, fileName, {}, metatypeDeclarations);
}

void Generator::generateServicesCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Services.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("../Impl.h")
        << QStringLiteral("Types_io.h") << QStringLiteral("<Log.h>")
        << QStringLiteral("<DurableService.h>") << QStringLiteral("<Helpers.h>")
        << QStringLiteral("<algorithm>") << QStringLiteral("<cmath>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx, QStringLiteral("Services.h"), additionalIncludes);

    const auto & services = parser.services();

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << ln << ln;
        generateServiceClassDeclaration(s, ServiceClassType::NonDurable, ctx);
        generateServiceClassDefinition(s, ctx);
    }

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << ln << ln;
        generateServiceClassDeclaration(s, ServiceClassType::Durable, ctx);
    }

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << ln << ln;
        generateDurableServiceClassDefinition(s, ctx);
    }

    ctx.m_out << blockSeparator << ln << ln;

    for(const auto & s: services)
    {
        ctx.m_out << "I" << s.m_name << " * new" << s.m_name << "(" << ln;

        auto serviceName = decapitalize(s.m_name);

        ctx.m_out << "    QString " << serviceName << "Url," << ln
            << "    IRequestContextPtr ctx," << ln
            << "    QObject * parent," << ln
            << "    IRetryPolicyPtr retryPolicy)" << ln
            << "{" << ln
            << "    if (ctx && ctx->maxRequestRetryCount() == 0)" << ln
            << "    {" << ln
            << "        return new " << s.m_name << "(" << serviceName
            << "Url, ctx);" << ln
            << "    }" << ln
            << "    else" << ln
            << "    {" << ln
            << "        if (!retryPolicy) {" << ln
            << "            retryPolicy = newRetryPolicy();" << ln
            << "        }" << ln << ln
            << "        return new Durable" << s.m_name << "(" << ln
            << "            std::make_shared<" << s.m_name << ">("
            << serviceName << "Url, ctx)," << ln
            << "            ctx," << ln
            << "            retryPolicy," << ln
            << "            parent);" << ln
            << "    }" << ln
            << "}" << ln
            << ln;
    }

    writeNamespaceEnd(ctx.m_out);

    ctx.m_out << ln;
    ctx.m_out << "#include <Services.moc>" << ln;
}

void Generator::generateServerHeader(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Servers.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("../RequestContext.h")
        << QStringLiteral("Constants.h")
        << QStringLiteral("Types.h")
        << QStringLiteral("<QObject>")
        << QStringLiteral("<functional>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    const auto & services = parser.services();
    for(const auto & s: services) {
        generateServerClassDeclaration(s, ctx);
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateServerCpp(Parser & parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Servers.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("../Thrift.h")
        << QStringLiteral("Types_io.h") << QStringLiteral("<Log.h>");
    sortIncludes(additionalIncludes);
    writeHeaderBody(ctx, QStringLiteral("Servers.h"), additionalIncludes);

    // First generate some helper functions

    ctx.m_out << "namespace {" << ln << ln;

    const auto & services = parser.services();
    for(const auto & s: services) {
        generateServerHelperFunctions(s, ctx);
    }

    ctx.m_out << "} // namespace" << ln << ln;

    // Now generate actual server classes

    for(const auto & s: services) {
        generateServerClassDefinition(s, ctx);
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateTestServerHeaders(
    Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../SocketHelpers.h") << QStringLiteral("<QObject>");
    sortIncludes(additionalIncludes);

    const auto & services = parser.services();
    for(const auto & s: services)
    {
        if (!s.m_extends.isEmpty()) {
            throw std::runtime_error("extending services is not supported");
        }

        const QString fileName = QStringLiteral("Test") + s.m_name +
            QStringLiteral(".h");

        OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

        writeHeaderHeader(ctx, fileName, additionalIncludes, HeaderKind::Private);

        ctx.m_out << blockSeparator << ln << ln;

        ctx.m_out << "class " << s.m_name << "Tester: public QObject" << ln
            << "{" << ln
            << "    Q_OBJECT" << ln
            << "public:" << ln
            << "    explicit " << s.m_name << "Tester(QObject * parent = nullptr);"
            << ln << ln
            << "private Q_SLOTS:" << ln;

        for(const auto & func: s.m_functions)
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
}

void Generator::generateTestServerCpps(Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../SocketHelpers.h")
        << QStringLiteral("RandomDataGenerators.h")
        << QStringLiteral("<generated/Servers.h>")
        << QStringLiteral("<generated/Services.h>")
        << QStringLiteral("<QTcpServer>")
        << QStringLiteral("<QtTest/QtTest>");
    sortIncludes(additionalIncludes);

    const auto & enumerations = parser.enumerations();
    const auto & services = parser.services();
    for(const auto & s: services)
    {
        if (!s.m_extends.isEmpty()) {
            throw std::runtime_error("extending services is not supported");
        }

        const QString fileName = QStringLiteral("Test") + s.m_name +
            QStringLiteral(".cpp");

        OutputFileContext ctx(fileName, outPath, OutputFileType::Test);

        writeHeaderBody(
            ctx,
            QStringLiteral("Test") + s.m_name + QStringLiteral(".h"),
            additionalIncludes,
            HeaderKind::Test);

        ctx.m_out << blockSeparator << ln << ln;

        ctx.m_out << s.m_name << "Tester::" << s.m_name << "Tester"
            << "(QObject * parent) :" << ln
            << "    QObject(parent)" << ln
            << "{}" << ln << ln;

        generateTestServerHelperClassDefinition(s, ctx);
        generateTestServerAsyncValueFetcherClassDefinition(s, ctx);

        for(const auto & func: s.m_functions)
        {
            ctx.m_out << blockSeparator << ln << ln;

            auto funcName = capitalize(func.m_name);

            // Should deliver request and response for successful synchronous
            // calls

            ctx.m_out << "void " << s.m_name << "Tester::shouldExecute"
                << funcName << "()" << ln;

            ctx.m_out << "{" << ln;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestResponse(func, enumerations, ctx);
            generateTestServerHelperLambda(s, func, parser, ctx);
            generateTestServerSocketSetup(s, func, ctx);
            generateTestServerServiceCall(s, func, ServiceCallKind::Sync, ctx);

            ctx.m_out << "}" << ln << ln;

            // Should deliver exceptions for synchronous calls

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "void " << s.m_name << "Tester::shouldDeliver"
                    << exceptionTypeName << "In" << funcName << "()" << ln;

                ctx.m_out << "{" << ln;

                generateTestServerPrepareRequestParams(func, enumerations, ctx);
                generateTestServerPrepareRequestExceptionResponse(parser, e, ctx);
                generateTestServerHelperLambda(s, func, parser, ctx, e.m_name);
                generateTestServerSocketSetup(s, func, ctx);

                generateTestServerServiceCall(
                    s, func, ServiceCallKind::Sync, ctx, exceptionTypeName,
                    e.m_name);

                ctx.m_out << "}" << ln << ln;
            }

            // Should also properly deliver ThriftExceptions in synchronous
            // calls

            ctx.m_out << "void " << s.m_name
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
                s, func, parser, ctx, exceptionField.m_name);

            generateTestServerSocketSetup(s, func, ctx);

            generateTestServerServiceCall(
                s, func, ServiceCallKind::Sync, ctx,
                QStringLiteral("ThriftException"), exceptionField.m_name);

            ctx.m_out << "}" << ln << ln;

            // Should deliver request and response for successful asynchonous
            // calls

            ctx.m_out << "void " << s.m_name << "Tester::shouldExecute"
                << funcName << "Async()" << ln;

            ctx.m_out << "{" << ln;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestResponse(func, enumerations, ctx);
            generateTestServerHelperLambda(s, func, parser, ctx);
            generateTestServerSocketSetup(s, func, ctx);
            generateTestServerServiceCall(s, func, ServiceCallKind::Async, ctx);

            ctx.m_out << "}" << ln << ln;

            // Should deliver exceptions for asynchronous calls

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "void " << s.m_name << "Tester::shouldDeliver"
                    << exceptionTypeName << "In" << funcName << "Async()" << ln;

                ctx.m_out << "{" << ln;

                generateTestServerPrepareRequestParams(func, enumerations, ctx);
                generateTestServerPrepareRequestExceptionResponse(parser, e, ctx);
                generateTestServerHelperLambda(s, func, parser, ctx, e.m_name);
                generateTestServerSocketSetup(s, func, ctx);

                generateTestServerServiceCall(
                    s, func, ServiceCallKind::Async, ctx, exceptionTypeName,
                    e.m_name);

                ctx.m_out << "}" << ln << ln;
            }

            // Should also properly deliver ThriftExceptions in synchronous
            // calls

            ctx.m_out << "void " << s.m_name
                << "Tester::shouldDeliverThriftExceptionIn" << funcName
                << "Async()" << ln;

            ctx.m_out << "{" << ln;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestExceptionResponse(
                parser, exceptionField, ctx);

            generateTestServerHelperLambda(
                s, func, parser, ctx, exceptionField.m_name);

            generateTestServerSocketSetup(s, func, ctx);

            generateTestServerServiceCall(
                s, func, ServiceCallKind::Async, ctx,
                QStringLiteral("ThriftException"), exceptionField.m_name);

            ctx.m_out << "}" << ln << ln;
        }

        writeNamespaceEnd(ctx.m_out);

        ctx.m_out << ln
            << "#include <Test" << s.m_name << ".moc>" << ln;
    }
}

void Generator::generateTestRandomDataGeneratorsHeader(
    Parser & parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("<generated/Types.h>");
    sortIncludes(additionalIncludes);

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

    for(const auto & s: parser.structures())
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "();" << ln
            << ln;
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

    for(const auto & s: parser.structures())
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

void Generator::generateTypeLocalDataAccessoryMethodDeclarations(
    const QString & className, OutputFileContext & ctx, QString indent)
{
    ctx.m_out << indent << "/**" << ln
        << indent
        << " * @brief localId can be used as a local unique identifier" << ln
        << indent
        << " * for any data item before it has been synchronized with" << ln
        << indent
        << " * Evernote and thus before it can be identified using its guid."
        << ln
        << indent << " *" << ln
        << indent << " * localId is generated automatically on" << ln
        << indent << " * construction for convenience but can be overridden "
        << "manually" << ln
        << indent << " */" << ln;

    ctx.m_out << indent << "[[nodiscard]] QString localId() const;" << ln
        << indent << "void setLocalId(QString id);" << ln << ln;

    ctx.m_out << indent << "/**" << ln
        << indent
        << " * @brief parentLocalId can be used as a local unique identifier"
        << ln << indent
        << " * of the data item being a parent to this data item." << ln
        << indent << " *" << ln
        << indent << " * For example, a note is a parent to a resource, a "
        << "notebook" << ln << indent
        << " * is a parent to a note. So note's localId is a parentLocalId "
        << "for a" << ln << indent
        << " * resource, notebook's localId is a parentLocalId for a note,"
        << ln << indent << " * tag's localId is a parentLocalId to a child tag."
        << ln
        << indent << " *" << ln
        << indent << " * By default the parentLocalId property is empty" << ln
        << indent << " */" << ln;

    ctx.m_out << indent << "[[nodiscard]] QString parentLocalId() const;" << ln
        << indent << "void setParentLocalId(QString id);" << ln << ln;

    ctx.m_out << indent << "/**" << ln
        << indent
        << " * @brief locallyModified flag can be used to keep track which"
        << ln
        << indent
        << " * objects have been modified locally and thus need to be "
        << "synchronized" << ln
        << indent << " * with Evernote service" << ln
        << indent << " */" << ln;

    ctx.m_out << indent << "[[nodiscard]] bool isLocallyModified() const;" << ln
        << indent << "void setLocallyModified(bool modified = true);"
        << ln << ln;

    ctx.m_out << indent << "/**" << ln
        << indent << " * @brief localOnly flag can be used to keep track which"
        << ln
        << indent << " * data items are meant to be local only and thus never "
        << "be synchronized" << ln
        << indent << " * with Evernote service" << ln
        << indent << " */" << ln;

    ctx.m_out << indent << "[[nodiscard]] bool isLocalOnly() const;" << ln
        << indent << "void setLocalOnly(bool localOnly = true);" << ln << ln;

    ctx.m_out << indent << "/**" << ln
        << indent
        << " * @brief locallyFavorited property can be used to keep track which"
        << ln << indent
        << " * data items were favorited in the client. Unfortunately," << ln
        << indent
        << " * Evernote has never provided a way to synchronize such" << ln
        << indent << " * a property between different clients" << ln
        << indent << " */" << ln;

    ctx.m_out << indent << "[[nodiscard]] bool isLocallyFavorited() const;"
        << ln << indent << "void setLocallyFavorited(bool favorited = true);"
        << ln << ln;
}

void Generator::generateTypeLocalDataAccessoryMethodDefinitions(
    const QString & className, OutputFileContext & ctx)
{
    constexpr const char * indent = "    ";

    ctx.m_out << "QString " << className << "::localId() const" << ln
        << "{" << ln
        << indent << "return d->m_localId;" << ln
        << "}" << ln << ln;

    ctx.m_out << "void " << className << "::setLocalId(QString id)" << ln
        << "{" << ln
        << indent << "d->m_localId = id;" << ln
        << "}" << ln << ln;

    ctx.m_out << "QString " << className << "::parentLocalId() const" << ln
        << "{" << ln
        << indent << "return d->m_parentLocalId;" << ln
        << "}" << ln << ln;

    ctx.m_out << "void " << className << "::setParentLocalId(QString id)" << ln
        << "{" << ln
        << indent << "d->m_parentLocalId = id;" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool " << className << "::isLocallyModified() const" << ln
        << "{" << ln
        << indent << "return d->m_locallyModified;" << ln
        << "}" << ln << ln;

    ctx.m_out << "void " << className << "::setLocallyModified("
        << "const bool modified)" << ln
        << "{" << ln
        << indent << "d->m_locallyModified = modified;" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool " << className << "::isLocalOnly() const" << ln
        << "{" << ln
        << indent << "return d->m_localOnly;" << ln
        << "}" << ln << ln;

    ctx.m_out << "void " << className << "::setLocalOnly(const bool localOnly)"
        << ln
        << "{" << ln
        << indent << "d->m_localOnly = localOnly;" << ln
        << "}" << ln << ln;

    ctx.m_out << "bool " << className << "::isLocallyFavorited() const" << ln
        << "{" << ln
        << indent << "return d->m_locallyFavorited;" << ln
        << "}" << ln << ln;

    ctx.m_out << "void " << className << "::setLocallyFavorited("
        << "const bool favorited)" << ln
        << "{" << ln
        << indent << "d->m_locallyFavorited = favorited;" << ln
        << "}" << ln << ln;
}

void Generator::generateClassAccessoryMethodsForFieldDeclarations(
    const Parser::Field & field, OutputFileContext & ctx, QString indent)
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
    ctx.m_out << indent << "void set" << capitalize(field.m_name) << "("
        << fieldTypeName << " " << field.m_name << ");" << ln;
}

void Generator::generateClassAccessoryMethodsForFieldDefinitions(
    const Parser::Structure & s, const Parser::Field & field,
    OutputFileContext & ctx)
{
    const QString fieldTypeName = fieldTypeToStr(field);
    const bool isPrimitiveType = isFieldOfPrimitiveType(field, fieldTypeName);
    constexpr const char * indent = "    ";

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
    ctx.m_out << "void " << s.m_name << "::set" << capitalize(field.m_name)
        << "(" << fieldTypeName << " " << field.m_name << ")" << ln
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
    ctx.m_out << "    Q_OBJECT" << ln;
    ctx.m_out << "    Q_DISABLE_COPY(" << className << ")" << ln;
    ctx.m_out << "public:" << ln;

    ctx.m_out << "    explicit " << className << "(" << ln;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "            QString " << serviceName << "Url = {},"
            << ln;
    }
    else {
        ctx.m_out << "            I" << service.m_name << "Ptr service,"
            << ln;
    }

    ctx.m_out << "            IRequestContextPtr ctx = {}," << ln;

    if (serviceClassType == ServiceClassType::Durable) {
        ctx.m_out << "            IRetryPolicyPtr retryPolicy = newRetryPolicy(),"
            << ln;
    }

    ctx.m_out << "            QObject * parent = nullptr) :" << ln
        << "        I" << service.m_name << "(parent)," << ln;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "        m_url(std::move(" << serviceName << "Url)),"
            << ln;
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
        << "        }" << ln;

    if (serviceClassType == ServiceClassType::Durable) {
        ctx.m_out << ln
            << "        m_service->setParent(this);" << ln;
    }

    ctx.m_out << "    }" << ln
        << ln;

    if (serviceClassType == ServiceClassType::NonDurable)
    {
        ctx.m_out << "    explicit " << className
            << "(QObject * parent) :" << ln
            << "        I" << service.m_name << "(parent)" << ln
            << "    {" << ln
            << "        m_ctx = newRequestContext();" << ln
            << "    }" << ln
            << ln;

        ctx.m_out << "    virtual void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << ln
            << "    {" << ln
            << "        m_url = std::move(" << serviceName << "Url);" << ln
            << "    }"
            << ln << ln;

        ctx.m_out << "    virtual QString " << serviceName
            << "Url() const override" << ln
            << "    {" << ln
            << "        return m_url;" << ln
            << "    }"
            << ln << ln;
    }
    else
    {
        ctx.m_out << "    ~" << className << "()" << ln
            << "    {" << ln
            << "        // Don't interfere with std::shared_ptr's lifetime"
            << " tracking" << ln
            << "        m_service->setParent(nullptr);" << ln
            << "    }" << ln << ln;

        ctx.m_out << "    virtual void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << ln
            << "    {" << ln
            << "        m_service->set" << service.m_name << "Url("
            << serviceName << "Url);" << ln
            << "    }"
            << ln << ln;

        ctx.m_out << "    virtual QString " << serviceName
            << "Url() const override" << ln
            << "    {" << ln
            << "        return m_service->" << serviceName << "Url();" << ln
            << "    }"
            << ln << ln;
    }

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    virtual " << typeToStr(func.m_type, func.m_name) << " "
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

        ctx.m_out << "    virtual AsyncResult * " << func.m_name
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

        ctx.m_out << (isVoidResult
                      ? QStringLiteral("void")
                      : typeToStr(f.m_type, f.m_name))
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
            ctx.m_out << "    " << readReplyName << "(reply);" << ln
                << "    return QVariant();" << ln;
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

        ctx.m_out << "AsyncResult * " << service.m_name << "::" << f.m_name
            << "Async(" << ln;
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
        ctx.m_out << ");" << ln << ln
            << "    return new AsyncResult(" << ln
            << "        m_url," << ln
            << "        params," << ln
            << "        ctx," << ln
            << "        " << asyncReadFunctionName << ");" << ln;

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
            << "        result.second->throwException();" << ln
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

        ctx.m_out << "AsyncResult * Durable" << service.m_name << "::"
            << func.m_name << "Async(" << ln;
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
            << "            return service->" << func.m_name << "Async("
            << ln;

        for(const auto & param : func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << ln;
        }
        ctx.m_out << "                ctx);" << ln
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

        ctx.m_out << "    return m_durableService->executeAsyncRequest("
            << ln
            << "        std::move(request), ctx);" << ln << ln;

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
        ctx.m_out << "        EverCloudExceptionDataPtr "
            << "exceptionData);" << ln << ln;
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
        ctx.m_out << "    EverCloudExceptionDataPtr exceptionData)"
            << ln;

        ctx.m_out << "{" << ln;

        ctx.m_out << "    ThriftBinaryBufferWriter writer;" << ln
            << "    qint32 cseqid = 0;" << ln << ln;

        ctx.m_out << "    if (exceptionData)" << ln
            << "    {" << ln
            << "        try" << ln
            << "        {" << ln
            << "            exceptionData->throwException();" << ln
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
            ctx.m_out << "    if (exceptionData)" << ln
                << "    {" << ln;

            ctx.m_out << "        try" << ln
                << "        {" << ln
                << "            exceptionData->throwException();" << ln
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
    generateTypesHeader(parser, outPath);
    generateTypesCpp(parser, outPath);

    generateTypeAliasesHeader(parser.typeAliases(), outPath);

    const QString typesSection = QStringLiteral("types");
    for (const auto & s: parser.structures())
    {
        generateTypeHeader(s, outPath, typesSection);
        generateTypeCpp(s, outPath, typesSection);
        generateTypeImplHeader(s, enumerations, outPath, typesSection);
        generateTypeImplCpp(s, outPath, typesSection);
    }

    const QString exceptionsSection = QStringLiteral("exceptions");
    for (const auto & s: parser.exceptions())
    {
        generateTypeHeader(s, outPath, exceptionsSection);
        generateTypeCpp(s, outPath, exceptionsSection);
        generateTypeImplHeader(s, enumerations, outPath, exceptionsSection);
        generateTypeImplCpp(s, outPath, exceptionsSection);
    }

    generateServicesHeader(parser, outPath);
    generateServicesCpp(parser, outPath);

    generateServerHeader(parser, outPath);
    generateServerCpp(parser, outPath);

    generateTestServerHeaders(parser, outPath);
    generateTestServerCpps(parser, outPath);

    generateTestRandomDataGeneratorsHeader(parser, outPath);
    generateTestRandomDataGeneratorsCpp(parser, outPath);
}

