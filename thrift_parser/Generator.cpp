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
#include <QString>
#include <QTextStream>

#include <algorithm>
#include <memory>
#include <stdexcept>

using namespace qevercloud;

namespace {

////////////////////////////////////////////////////////////////////////////////

constexpr const char * ln = "\n";

////////////////////////////////////////////////////////////////////////////////

static const char * disclaimer =
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

static const char * blockSeparator = "/////////////////////////////////////////"
                                     "///////////////////////////////////////";

////////////////////////////////////////////////////////////////////////////////

QString generatedFileOutputPath(
    const QString & outPath, const OutputFileType type)
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

    return path;
}

void ensureFileDirExists(const QString & path)
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
    const OutputFileType type)
{
    QString path = generatedFileOutputPath(outPath, type);
    ensureFileDirExists(path);

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

QString Generator::clearTypedef(const QString & s) const
{
    if (m_typedefMap.contains(s)) {
        return m_typedefMap.value(s);
    }

    return s;
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
    auto baseType = std::dynamic_pointer_cast<Parser::BaseType>(field.m_type);
    if (baseType)
    {
        out << prefix;
        if (!field.m_name.isEmpty()) {
            out << field.m_name << " = ";
        }

        out << getGenerateRandomValueFunction(baseType->m_baseType) << end;
        return;
    }

    auto identifierType = std::dynamic_pointer_cast<Parser::IdentifierType>(
        field.m_type);
    if (identifierType)
    {
        out << prefix;
        if (!field.m_name.isEmpty()) {
            out << field.m_name << " = ";
        }

        auto actualType = clearInclude(identifierType->m_identifier);
        actualType = clearTypedef(actualType);

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
            out << actualType << "::" << e.m_values[index].first << end;
        }
        else if (exceptionIt != exceptions.end())
        {
            generateGetRandomExceptionExpression(
                field, *exceptionIt, prefix, parser, out);
        }
        else
        {
            out << getGenerateRandomValueFunction(actualType) << end;
        }

        return;
    }

    auto listType = std::dynamic_pointer_cast<Parser::ListType>(field.m_type);
    if (listType)
    {
        verifyTypeIsBaseOrIdentifier(listType->m_valueType);

        auto valueType = typeToStr(
            listType->m_valueType,
            {},
            MethodType::TypeName);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            out << prefix << field.m_name << " = QList<"
                << valueType << ">();" << ln;
        }

        valueType = clearInclude(valueType);
        valueType = clearTypedef(valueType);

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << field.m_name;

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                out << ".ref()";
            }

            out << " << ";

            Parser::Field pseudoField;
            pseudoField.m_type = listType->m_valueType;
            pseudoField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(pseudoField, {}, parser, out);
        }

        return;
    }

    auto setType = std::dynamic_pointer_cast<Parser::SetType>(field.m_type);
    if (setType)
    {
        verifyTypeIsBaseOrIdentifier(setType->m_valueType);

        auto valueType = typeToStr(
            setType->m_valueType,
            {},
            MethodType::TypeName);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            out << prefix << field.m_name << " = QSet<"
                << valueType << ">();" << ln;
        }

        valueType = clearInclude(valueType);
        valueType = clearTypedef(valueType);

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << field.m_name;

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

            QString setItemEnd = QStringLiteral(");\n");
            generateGetRandomValueExpression(
                pseudoField,
                {},
                parser,
                out,
                setItemEnd);
        }

        return;
    }

    auto mapType = std::dynamic_pointer_cast<Parser::MapType>(field.m_type);
    if (mapType)
    {
        verifyTypeIsBaseOrIdentifier(mapType->m_keyType);
        verifyTypeIsBaseOrIdentifier(mapType->m_valueType);

        auto keyType = typeToStr(
            mapType->m_keyType,
            {},
            MethodType::TypeName);

        auto valueType = typeToStr(
            mapType->m_valueType,
            {},
            MethodType::TypeName);

        if (field.m_required == Parser::Field::RequiredFlag::Optional) {
            out << prefix << field.m_name << " = QMap<"
                << keyType << ", " << valueType << ">();" << ln;
        }

        keyType = clearInclude(keyType);
        keyType = clearTypedef(keyType);

        valueType = clearInclude(valueType);
        valueType = clearTypedef(valueType);

        for(size_t i = 0; i < 3; ++i)
        {
            out << prefix << field.m_name;

            if (field.m_required == Parser::Field::RequiredFlag::Optional) {
                out << ".ref()";
            }

            out << "[";

            Parser::Field pseudoKeyField;
            pseudoKeyField.m_type = mapType->m_keyType;
            pseudoKeyField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(pseudoKeyField, {}, parser, out, {});

            out << "] = ";

            Parser::Field pseudoValueField;
            pseudoValueField.m_type = mapType->m_valueType;
            pseudoValueField.m_required = Parser::Field::RequiredFlag::Required;

            generateGetRandomValueExpression(pseudoValueField, {}, parser, out);
        }

        return;
    }

    throw std::runtime_error(
        "Unsupported field type: " +
        typeToStr(field.m_type, {}, MethodType::TypeName).toStdString());
}

void Generator::verifyTypeIsBaseOrIdentifier(
    const std::shared_ptr<Parser::Type> & type) const
{
    if (std::dynamic_pointer_cast<Parser::BaseType>(type) != nullptr &&
        std::dynamic_pointer_cast<Parser::IdentifierType>(type) != nullptr)
    {
        auto typeName = typeToStr(type, {}, MethodType::TypeName);
        throw std::runtime_error(
            "Unsupported type: expecting base or identifier type: " +
            typeName.toStdString());
    }
}

void Generator::generateGetRandomExceptionExpression(
    const Parser::Field & field,
    const Parser::Structure & e,
    const QString & prefix,
    const Parser & parser,
    QTextStream & out)
{
    out << e.m_name << "();" << ln;

    QString fieldPrefix = prefix + field.m_name;
    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        fieldPrefix += QStringLiteral("->");
    }
    else {
        fieldPrefix += QStringLiteral(".");
    }

    for(const auto & f: e.m_fields) {
        generateGetRandomValueExpression(f, fieldPrefix, parser, out);
    }
}

void Generator::generateGetThriftExceptionExpression(
    QTextStream & out)
{
    out << "ThriftException(" << ln
        << "        ThriftException::Type::INTERNAL_ERROR," << ln
        << "        QStringLiteral(\"Internal error\"));" << ln;
}

QString Generator::getGenerateRandomValueFunction(const QString & typeName) const
{
    if (typeName == QStringLiteral("bool"))
    {
        return QStringLiteral("generateRandomBool()");
    }
    else if ( (typeName == QStringLiteral("QString")) ||
              (typeName == QStringLiteral("string")) )
    {
        return QStringLiteral("generateRandomString()");
    }
    else if (typeName == QStringLiteral("double"))
    {
        return QStringLiteral("generateRandomDouble()");
    }
    else if ( (typeName == QStringLiteral("QByteArray")) ||
              (typeName == QStringLiteral("binary")) )
    {
        return QStringLiteral("generateRandomString().toUtf8()");
    }
    else if ( (typeName == QStringLiteral("quint8")) ||
              (typeName == QStringLiteral("byte")) ||
              (typeName == QStringLiteral("char")) )
    {
        return QStringLiteral("generateRandomUint8()");
    }
    else if ( (typeName == QStringLiteral("qint16")) ||
              (typeName == QStringLiteral("i16")) )
    {
        return QStringLiteral("generateRandomInt16()");
    }
    else if ( (typeName == QStringLiteral("qint32")) ||
              (typeName == QStringLiteral("i32")) )
    {
        return QStringLiteral("generateRandomInt32()");
    }
    else if ( (typeName == QStringLiteral("qint64")) ||
              (typeName == QStringLiteral("i64")) )
    {
        return QStringLiteral("generateRandomInt64()");
    }
    else {
        return QStringLiteral("generateRandom") + typeName + QStringLiteral("()");
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

void Generator::writeStructPrintDefinition(
    QTextStream & out, const Parser::Structure & s,
    const Parser & parser) const
{
    out << "void " << s.m_name << "::print(QTextStream & strm) const"
        << ln
        << "{" << ln;

    out << "    strm << \"" << s.m_name << ": {\\n\";" << ln;

    const auto & exceptions = parser.exceptions();
    auto exceptionIt = std::find_if(
        exceptions.begin(),
        exceptions.end(),
        [&] (const Parser::Structure & e)
        {
            return e.m_name == s.m_name;
        });
    if (exceptionIt == exceptions.end()) {
        out << "    localData.print(strm);" << ln;
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

            out << "    if (" << f.m_name << ".isSet()) {" << ln
                << "        strm << \"    " << f.m_name << " = \"" << ln;

            if (mapType)
            {
                out << "            << \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << ln;
                out << "        for(const auto & it: toRange(" << f.m_name
                    << ".ref())) {" << ln;
                out << "            strm << \"        [\" << it.key() << \"] = \" "
                    << "<< it.value() << \"\\n\";" << ln;
                out << "        }" << ln;
                out << "        strm << \"    }\\n\";" << ln;
            }
            else if (setType)
            {
                out << "            << \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << ln;
                out << "        for(const auto & v: " << f.m_name
                    << ".ref()) {" << ln;
                out << "            strm << \"        \" << v << \"\\n\";" << ln;
                out << "        }" << ln;
                out << "        strm << \"    }\\n\";" << ln;
            }
            else if (listType)
            {
                out << "            << \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << ln;
                out << "        for(const auto & v: " << f.m_name
                    << ".ref()) {" << ln;
                out << "            strm << \"        \" << v << \"\\n\";" << ln;
                out << "        }" << ln;
                out << "        strm << \"    }\\n\";" << ln;
            }
            else
            {
                out << "            << "
                    << f.m_name << ".ref() << \"\\n\";" << ln;
            }

            out << "    }" << ln
                << "    else {" << ln
                << "        strm << \"    " << f.m_name << " is not set\\n\";"
                << ln
                << "    }" << ln << ln;
            previousOptional = true;
        }
        else
        {
            out << "    strm << \"    " << f.m_name << " = \"" << ln;

            if (mapType)
            {
                out << "        << \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << ln;
                out << "    for(const auto & it: toRange(" << f.m_name
                    << ")) {" << ln;
                out << "        strm << \"    [\" << it.key() << \"] = \" "
                    << "<< it.value() << \"\\n\";" << ln;
                out << "    }" << ln;
                out << "    strm << \"}\\n\";" << ln;
            }
            else if (setType)
            {
                out << "        << \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << ln;
                out << "    for(const auto & v: " << f.m_name
                    << ") {" << ln;
                out << "        strm << \"    \" << v << \"\\n\";" << ln;
                out << "    }" << ln;
                out << "    strm << \"}\\n\";" << ln;
            }
            else if (listType)
            {
                out << "        << \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << ln;
                out << "    for(const auto & v: " << f.m_name
                    << ") {" << ln;
                out << "        strm << \"    \" << v << \"\\n\";" << ln;
                out << "    }" << ln;
                out << "    strm << \"}\\n\";" << ln;
            }
            else
            {
                out << "        << "
                    << f.m_name << " << \"\\n\";" << ln;
            }

            previousOptional = false;
        }
    }

    out << "    strm << \"}\\n\";" << ln
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

        auto paramTypeName = typeToStr(
            param.m_type,
            {},
            MethodType::TypeName);

        auto baseType = std::dynamic_pointer_cast<Parser::BaseType>(param.m_type);
        auto identifierType = std::dynamic_pointer_cast<Parser::IdentifierType>(
            param.m_type);

        QString actualParamTypeName;

        if (baseType) {
            actualParamTypeName = baseType->m_baseType;
        }
        else if (identifierType) {
            actualParamTypeName = clearInclude(identifierType->m_identifier);
            actualParamTypeName = clearTypedef(actualParamTypeName);
        }
        else {
            throw std::runtime_error("Unsupported parameter type: " +
                                     paramTypeName.toStdString());
        }

        ctx.m_out << "    " << paramTypeName << " " << param.m_name
            << " = ";

        auto enumIt = std::find_if(
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
        actualValueType = clearTypedef(actualValueType);

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
        actualValueType = clearTypedef(actualValueType);

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
        actualKeyType = clearTypedef(actualKeyType);

        auto actualValueType = clearInclude(valueType);
        actualValueType = clearTypedef(actualValueType);

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
        actualResponseTypeName = clearTypedef(actualResponseTypeName);

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
        ctx.m_out << "    auto " << e.m_name << " = ";
        generateGetThriftExceptionExpression(ctx.m_out);
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

    ctx.m_out << "    auto " << e.m_name << " = ";

    const QString prefix = QStringLiteral("    ");
    generateGetRandomExceptionExpression(
        e,
        *exceptionIt,
        prefix,
        parser,
        ctx.m_out);

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

        auto baseType = std::dynamic_pointer_cast<Parser::BaseType>(param.m_type);
        if (!baseType ||
            baseType->m_baseType == QStringLiteral("string"))
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
        << "readRequestBodyFromSocket(*pSocket);" << ln
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
    const HeaderKind headerKind)
{
    ctx.m_out << disclaimer << ln;

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());
    ctx.m_out << "#ifndef " << guard << ln;
    ctx.m_out << "#define " << guard << ln;
    ctx.m_out << ln;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include \"../Export.h\"" << ln;
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
    auto baseType = std::dynamic_pointer_cast<Parser::BaseType>(type);
    auto voidType = std::dynamic_pointer_cast<Parser::VoidType>(type);
    auto identifierType = std::dynamic_pointer_cast<Parser::IdentifierType>(type);
    auto mapType = std::dynamic_pointer_cast<Parser::MapType>(type);
    auto setType = std::dynamic_pointer_cast<Parser::SetType>(type);
    auto listType = std::dynamic_pointer_cast<Parser::ListType>(type);

    QString result;

    QString typeName;
    if (methodType == MethodType::FuncParamType) {
        typeName = typeToStr(type, identifier, MethodType::TypeName);
    }

    if (baseType)
    {
        if (baseType->m_baseType == QStringLiteral("bool"))
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
        else if (baseType->m_baseType == QStringLiteral("string"))
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
        else if (baseType->m_baseType == QStringLiteral("double"))
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
        else if (baseType->m_baseType == QStringLiteral("binary"))
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
        else if (baseType->m_baseType == QStringLiteral("byte"))
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
        else if (baseType->m_baseType == QStringLiteral("i16"))
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
        else if (baseType->m_baseType == QStringLiteral("i32"))
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
        else if (baseType->m_baseType == QStringLiteral("i64"))
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
        QString nameOfType = clearInclude(identifierType->m_identifier);
        if (methodType == MethodType::FuncParamType)
        {
            if (m_allEnums.contains(nameOfType))
            {
                result = typeName;
            }
            else
            {
                QString nameOfType2 = clearTypedef(nameOfType);
                if (nameOfType2 != nameOfType) {
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
            result = nameOfType;
        }
        else if (methodType == MethodType::ReadTypeName)
        {
            result = (nameOfType == QStringLiteral("Timestamp")
                      ? QStringLiteral("qint64")
                      : nameOfType);
        }
        else
        {
            QString nameOfType2 = clearTypedef(nameOfType);
            if (nameOfType2 != nameOfType)
            {
                if (!m_baseTypes.contains(nameOfType2)) {
                    throw std::runtime_error(
                        "typedefs are supported for base types only");
                }

                std::shared_ptr<Parser::BaseType> type2(new Parser::BaseType);
                type2->m_baseType = nameOfType2;
                result = typeToStr(type2, identifier, methodType);
            }
            else
            {
                if (m_allStructs.contains(nameOfType) ||
                    m_allExceptions.contains(nameOfType))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        {
                            QTextStream strm(&result);
                            strm << "write" << nameOfType << "(writer, ";
                        }
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "read" << nameOfType << "(reader, ";
                        }
                        break;
                    case MethodType::ThriftFieldType:
                        result = QStringLiteral("ThriftFieldType::T_STRUCT");
                        break;
                    default:
                        result = QLatin1String("");
                    }
                }
                else if (m_allEnums.contains(nameOfType))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        result = QStringLiteral("writer.writeI32(static_cast<qint32>(");
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "readEnum" << nameOfType << "(reader, ";
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
         methodType == MethodType::FuncParamType))
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
    Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Constants.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    writeHeaderHeader(ctx, fileName);

    ctx.m_out << blockSeparator << ln << ln;

    const auto & constants = parser->constants();
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

void Generator::generateConstantsCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Constants.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("<Helpers.h>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx, QStringLiteral("Constants.h"), additionalIncludes);

    ctx.m_out << blockSeparator << ln << ln;

    const auto & constants = parser->constants();
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

    auto baseType = std::dynamic_pointer_cast<Parser::BaseType>(
        field.m_type);
    if (baseType) {
        typeName = baseType->m_baseType;
    }
    else {
        typeName = clearTypedef(typeName);
    }

    if ( (typeName == QStringLiteral("byte")) ||
         (typeName == QStringLiteral("i16")) ||
         (typeName == QStringLiteral("i32")) ||
         (typeName == QStringLiteral("i64")) )
    {
        s += QStringLiteral(" = 0");
        return s;
    }
    else if (typeName == QStringLiteral("double"))
    {
        s += QStringLiteral(" = 0.0");
        return s;
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
    const QString & identPrefix, const QString & fieldPrefix)
{
    for(const auto & field: fields)
    {
        QString ident = QLatin1String("");

        bool isOptional =
            (field.m_required == Parser::Field::RequiredFlag::Optional);
        if (isOptional) {
            ident = QStringLiteral("    ");
            out << "    if (" << fieldPrefix
                << field.m_name << ".isSet()) {" << ln;
        }

        out << ident << "    writer.writeFieldBegin(" << ln
            << ident << "        QStringLiteral(\""
            << field.m_name << "\")," << ln
            << ident << "        " << typeToStr(
                field.m_type, identPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << "," << ln
            << ident << "        " << field.m_id << ");" << ln << ln;

        QString fieldMoniker;
        {
            QTextStream strm(&fieldMoniker);
            strm << fieldPrefix << field.m_name
                << (isOptional ? QStringLiteral(".ref()") : QLatin1String(""));
        }

        QString writeMethod = typeToStr(
            field.m_type, identPrefix + QStringLiteral(",") + field.m_name,
            MethodType::WriteMethod);

        if (writeMethod.contains(QStringLiteral("writeListBegin")))
        {
            auto valueType =
                std::dynamic_pointer_cast<Parser::ListType>(field.m_type)->m_valueType;

            out << ident << "    writer.writeListBegin("
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldMoniker << ".length());" << ln;

            out << ident
                << "    for(const auto & value: qAsConst("
                << fieldMoniker << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << "        " << writeMethod << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << ident << "    }" << ln;
            out << ident << "    writer.writeListEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            auto valueType =
                std::dynamic_pointer_cast<Parser::SetType>(field.m_type)->m_valueType;

            out << ident << "    writer.writeSetBegin("
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldMoniker
                << ".count());" << ln;

            out << ident << "    for(const auto & value: qAsConst("
                << fieldMoniker << ")) {" << ln;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << "        " << writeMethod
                << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << ident << "    }" << ln;
            out << ident << "    writer.writeSetEnd();" << ln << ln;
        }
        else if (writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            auto keyType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_keyType;

            auto valueType =
                std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_valueType;

            out << ident << "    writer.writeMapBegin("
                << typeToStr(
                    keyType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", "
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << ", " << fieldMoniker
                << ".size());" << ln;

            out << ident << "    for(const auto & it: "
                << "toRange(" << fieldMoniker
                << ")) {" << ln;

            QString keyWriteMethod = typeToStr(
                keyType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            QString valueWriteMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << "        " << keyWriteMethod
                << "it.key()"
                << (keyWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;
            out << ident << "        " << valueWriteMethod << "it.value()"
                << (valueWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;

            out << ident << "    }" << ln;
            out << ident << "    writer.writeMapEnd();" << ln << ln;
        }
        else
        {
            out << ident << "    " << writeMethod << fieldMoniker
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1String(""))
                << ");" << ln;
        }

        out << ident << "    writer.writeFieldEnd();" << ln;
        if (isOptional) {
            out << "    }" << ln;
        }
        out << ln;
    }
}

void Generator::writeThriftReadField(
    QTextStream & out, const Parser::Field & field, const QString & identPrefix,
    const QString & fieldParent)
{
    const char * indent = "                ";

    out << indent
        << typeToStr(
            field.m_type, identPrefix + field.m_name, MethodType::ReadTypeName)
        << " v;" << ln;

    QString readMethod = typeToStr(
        field.m_type, identPrefix + field.m_name, MethodType::ReadMethod);
    if (readMethod.contains(QStringLiteral("readListBegin")))
    {
        auto valueType =
            std::dynamic_pointer_cast<Parser::ListType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType,  identPrefix + field.m_name,
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
            << identPrefix + field.m_name << ")\"));" << ln
            << indent << "}" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {"
            << ln;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
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
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << ln;
        out << indent << "ThriftFieldType elemType;" << ln;
        out << indent << "reader.readSetBegin(elemType, size);" << ln;
        out << indent << "v.reserve(size);" << ln;
        out << indent << "if (elemType != " << valueThriftType
            << ") {" << ln
            << indent << "    throw ThriftException(" << ln
            << indent << "        ThriftException::Type::INVALID_DATA," << ln
            << indent << "        QStringLiteral(\"Incorrect set type ("
            << identPrefix + field.m_name << ")\"));" << ln
            << indent << "}" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
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
            keyType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString keyThriftType = typeToStr(
            keyType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        auto valueType =
            std::dynamic_pointer_cast<Parser::MapType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << ln;
        out << indent << "ThriftFieldType keyType;" << ln;
        out << indent << "ThriftFieldType elemType;" << ln;
        out << indent << "reader.readMapBegin(keyType, elemType, size);" << ln;
        out << indent << "if (keyType != " << keyThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map key type ("
            << identPrefix << field.m_name << ")\"));" << ln;
        out << indent << "if (elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map value type ("
            << identPrefix + field.m_name << ")\"));" << ln;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << ln;
        out << indent << "    "
            << typeToStr(
                keyType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " key;" << ln;
        out << indent << "    " << keyReadMethod << "key);" << ln;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
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

    out << indent << fieldParent << field.m_name << " = v;" << ln;
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

void Generator::generateErrorsHeader(Parser * parser, const QString & outPath)
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

    const auto & enumerations = parser->enumerations();
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

void Generator::generateErrorsCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("EDAMErrorCode.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    writeHeaderBody(ctx, QStringLiteral("EDAMErrorCode.h"));

    ctx.m_out << blockSeparator << ln << ln;

    const auto & enumerations = parser->enumerations();
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

void Generator::generateTypesIOHeader(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types_io.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList()
        << QStringLiteral("<generated/Types.h>") << QStringLiteral("../Impl.h")
        << QStringLiteral("<Optional.h>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(
        ctx, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << ln << ln;

    QList<const QList<Parser::Structure>*> lists;
    lists.reserve(2);
    lists << &parser->structures();
    lists << &parser->exceptions();

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

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations) {
        ctx.m_out << "void readEnum" << e.m_name
            << "(ThriftBinaryBufferReader & reader, "
            << e.m_name << " & e);" << ln;
    }
    ctx.m_out << ln << "/** @endcond */" << ln;

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTypesHeader(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("EDAMErrorCode.h") << QStringLiteral("../Printable.h")
        << QStringLiteral("../Optional.h") << QStringLiteral("<QHash>")
        << QStringLiteral("<QMetaType>") << QStringLiteral("<QList>")
        << QStringLiteral("<QMap>") << QStringLiteral("<QSet>")
        << QStringLiteral("<QStringList>") << QStringLiteral("<QByteArray>")
        << QStringLiteral("<QDateTime>") << QStringLiteral("<QMetaType>")
        << QStringLiteral("<QVariant>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    const auto & typedefs = parser->typedefs();
    for(const auto & t: typedefs)
    {
        if (!t.m_docComment.isEmpty()) {
            ctx.m_out << t.m_docComment << ln;
        }

        ctx.m_out << "using " << t.m_name << " = "
            << typeToStr(t.m_type, t.m_name) << ";" << ln << ln;
    }
    ctx.m_out << ln;

    QSet<QString> safe;
    QList<Parser::Structure> ordered;

    QSet<QString> exceptions;
    const auto & parserExceptions = parser->exceptions();
    for(const auto & e: parserExceptions) {
        exceptions.insert(e.m_name);
    }

    auto heap = parser->structures();
    heap.append(parser->exceptions());

    int count = heap.count();
    while(!heap.isEmpty())
    {
        int i = 0;
        while(i < heap.count())
        {
            const Parser::Structure s = heap[i];
            bool safeStruct = true;
            for(const auto & f : s.m_fields)
            {
                QString typeName = getIdentifier(f.m_type);
                QString typeName2;
                if (typeName.isEmpty())
                {
                    auto listType =
                        std::dynamic_pointer_cast<Parser::ListType>(f.m_type);
                    auto setType =
                        std::dynamic_pointer_cast<Parser::SetType>(f.m_type);
                    auto mapType =
                        std::dynamic_pointer_cast<Parser::MapType>(f.m_type);

                    if (setType) {
                        typeName = getIdentifier(setType->m_valueType);
                    }
                    else if (listType) {
                        typeName = getIdentifier(listType->m_valueType);
                    }
                    else if (mapType) {
                        typeName = getIdentifier(mapType->m_valueType);
                        typeName2 = getIdentifier(mapType->m_keyType);
                    }
                }

                if (!typeName.isEmpty() &&
                    (m_allStructs.contains(typeName) ||
                     m_allExceptions.contains(typeName)) &&
                    !safe.contains(typeName))
                {
                    safeStruct = false;
                    break;
                }

                if (!typeName2.isEmpty() &&
                    (m_allStructs.contains(typeName2) ||
                     m_allExceptions.contains(typeName2))
                    && !safe.contains(typeName2))
                {
                    safeStruct = false;
                    break;
                }
            }

            if (safeStruct) {
                safe << s.m_name;
                ordered << s;
                heap.removeAt(i);
            }
            else {
                ++i;
            }
        }

        if (count == heap.count()) {
            throw std::runtime_error("Struct sorting is in infinite loop!");
        }
    }

    generateLocalDataClassDeclaration(ctx);
    ctx.m_out << ln;

    for(const auto & s: qAsConst(ordered))
    {
        if (!s.m_docComment.isEmpty()) {
            ctx.m_out << s.m_docComment << ln;
        }
        else {
            ctx.m_out << "/** NO DOC COMMENT ID FOUND */" << ln;
        }

        if (exceptions.contains(s.m_name))
        {
            ctx.m_out << "class QEVERCLOUD_EXPORT " << s.m_name
                << ": public EvernoteException, public Printable"
                << ln << "{" << ln
                << "    Q_GADGET" << ln
                << "public:" << ln;

            for(const auto & f : s.m_fields) {
                ctx.m_out << "    " << fieldDeclarationToStr(f) << ";" << ln;
            }

            ctx.m_out << ln;
            ctx.m_out << "    " << s.m_name
                << "();" << ln;
            ctx.m_out << "    virtual ~" << s.m_name
                << "() noexcept override;" << ln;

            if (!s.m_fields.isEmpty()) {
                ctx.m_out << ln;
                ctx.m_out << "    " << s.m_name
                    << "(const " << s.m_name
                    << " & other);" << ln;
            }

            ctx.m_out << "    const char * what() const noexcept override;"
                << ln;
            ctx.m_out << "    virtual EverCloudExceptionDataPtr "
                << "exceptionData() const override;" << ln << ln;
            ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
                << ln;
        }
        else
        {
            ctx.m_out << "struct QEVERCLOUD_EXPORT "
                << s.m_name << ": public Printable" << ln
                << "{" << ln
                << "private:" << ln
                << "    Q_GADGET" << ln
                << "public:" << ln;

            ctx.m_out << "    /**" << ln
                << "     * See the declaration of EverCloudLocalData for details"
                << ln
                << "     */" << ln
                << "    EverCloudLocalData localData;" << ln << ln;

            for(const auto & f: qAsConst(s.m_fields))
            {
                if (s.m_fieldComments.contains(f.m_name))
                {
                    auto lines =
                        s.m_fieldComments[f.m_name].split(QStringLiteral("\n"));
                    for(const auto & line: qAsConst(lines)) {
                        ctx.m_out << "    " << line << ln;
                    }
                }
                else
                {
                    ctx.m_out << "    /** NOT DOCUMENTED */" << ln;
                }

                ctx.m_out << "    " << fieldDeclarationToStr(f) << ";" << ln;
            }

            ctx.m_out << ln;
            ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
                << ln;
        }

        ctx.m_out << ln;
        ctx.m_out << QString::fromUtf8(
            "    bool operator==(const %1 & other) const").arg(s.m_name) << ln;
        ctx.m_out << "    {" << ln;

        bool first = true;
        for(const auto & f : s.m_fields)
        {
            if (first) {
                first = false;
                ctx.m_out << "        return ";
            }
            else {
                ctx.m_out << "            && ";
            }

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << QString::fromUtf8("%1.isEqual(other.%1)").arg(f.m_name)
                    << ln;
            }
            else {
                ctx.m_out << QString::fromUtf8("(%1 == other.%1)").arg(f.m_name)
                    << ln;
            }
        }

        ctx.m_out << "        ;" << ln << "    }" << ln << ln;
        ctx.m_out << QString::fromUtf8(
            "    bool operator!=(const %1 & other) const").arg(s.m_name)
            << ln;
        ctx.m_out << "    {" << ln;
        ctx.m_out << "        return !(*this == other);" << ln;
        ctx.m_out << "    }" << ln;

        ctx.m_out << ln;

        QHash<QString, QString> typeDefs;
        for(const auto & f: s.m_fields)
        {
            auto fieldTypeName = typeToStr(
                f.m_type,
                {},
                MethodType::TypeName);

            if (fieldTypeName.contains(QStringLiteral(","))) {
                // In earlier versions of Qt Q_PROPERTY macro can't handle type names
                // containing comma
                auto typeDef = capitalize(f.m_name);
                typeDefs[fieldTypeName] = typeDef;
            }
        }

        for(auto it = typeDefs.constBegin(), end = typeDefs.constEnd();
            it != end; ++it)
        {
            ctx.m_out << "    using " << it.value() << " = "
                << it.key() << ";" << ln;
        }

        if (!typeDefs.isEmpty()) {
            ctx.m_out << ln;
        }

        if (!exceptions.contains(s.m_name)) {
            ctx.m_out << "    Q_PROPERTY(EverCloudLocalData localData MEMBER "
                << "localData)" << ln;
        }

        for(const auto & f: s.m_fields)
        {
            auto fieldTypeName = typeToStr(
                f.m_type,
                {},
                MethodType::TypeName);

            auto it = typeDefs.find(fieldTypeName);
            if (it != typeDefs.end()) {
                fieldTypeName = it.value();
            }

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                fieldTypeName = QStringLiteral("Optional<") +
                    fieldTypeName + QStringLiteral(">");
            }

            ctx.m_out << "    Q_PROPERTY(" << fieldTypeName
                << " " << f.m_name << " MEMBER " << f.m_name << ")" << ln;
        }

        ctx.m_out << "};" << ln << ln;
    }

    QStringList extraLinesOutsideNamespace;
    extraLinesOutsideNamespace.reserve(ordered.size() + 2);

    Parser::Structure everCloudLocalDataStruct;
    everCloudLocalDataStruct.m_name = QStringLiteral("EverCloudLocalData");
    ordered.prepend(everCloudLocalDataStruct);

    for(const auto & s: qAsConst(ordered)) {
        QString line;
        QTextStream lineOut(&line);

        lineOut << "Q_DECLARE_METATYPE(qevercloud::" << s.m_name << ")";
        lineOut.flush();

        extraLinesOutsideNamespace << line;
    }
    extraLinesOutsideNamespace << QString();

    writeHeaderFooter(ctx.m_out, fileName, {}, extraLinesOutsideNamespace);
}

void Generator::generateTypesCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Types.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("../Impl.h")
        << QStringLiteral("Types_io.h") << QStringLiteral("<Helpers.h>")
        << QStringLiteral("<QUuid>") << QStringLiteral("<QDebug>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx, QStringLiteral("Types.h"), additionalIncludes);

    ctx.m_out << blockSeparator << ln << ln;
    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << ln << ln;

    const auto & enumerations = parser->enumerations();
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
    for(const auto & e: parser->exceptions()) {
        exceptions.insert(e.m_name);
    }

    auto structsAndExceptions = parser->structures();
    structsAndExceptions << parser->exceptions();

    generateLocalDataClassDefinition(ctx);
    ctx.m_out << ln;

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        if (exceptions.contains(s.m_name))
        {
            ctx.m_out << s.m_name << "::" << s.m_name
                << "() {}" << ln;
            ctx.m_out << s.m_name << "::~" << s.m_name
                << "() noexcept {}" << ln;

            if (!s.m_fields.isEmpty())
            {
                ctx.m_out << s.m_name << "::" << s.m_name
                    << "(const " << s.m_name
                    << "& other) : EvernoteException(other)" << ln;
                ctx.m_out << "{" << ln;
                for(const auto & f : s.m_fields) {
                    ctx.m_out << "   " << f.m_name
                        << " = other." << f.m_name
                        << ";" << ln;
                }
                ctx.m_out << "}" << ln;
            }
        }

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

        writeStructPrintDefinition(ctx.m_out, s, *parser);
        ctx.m_out << blockSeparator << ln << ln;
    }

    ctx.m_out << "/** @endcond */" << ln << ln;

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateServicesHeader(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Services.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("../AsyncResult.h")
        << QStringLiteral("../DurableService.h")
        << QStringLiteral("../RequestContext.h")
        << QStringLiteral("../Optional.h")
        << QStringLiteral("Constants.h")
        << QStringLiteral("Types.h")
        << QStringLiteral("<QObject>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    const auto & services = parser->services();
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

void Generator::generateServicesCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Services.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("../Impl.h")
        << QStringLiteral("Types_io.h") << QStringLiteral("<Log.h>")
        << QStringLiteral("<DurableService.h>") << QStringLiteral("<Helpers.h>")
        << QStringLiteral("<algorithm>") << QStringLiteral("<cmath>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx, QStringLiteral("Services.h"), additionalIncludes);

    const auto & services = parser->services();

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

void Generator::generateServerHeader(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Servers.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("../RequestContext.h")
        << QStringLiteral("../Optional.h")
        << QStringLiteral("Constants.h")
        << QStringLiteral("Types.h")
        << QStringLiteral("<QObject>")
        << QStringLiteral("<functional>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx, fileName, additionalIncludes);

    const auto & services = parser->services();
    for(const auto & s: services) {
        generateServerClassDeclaration(s, ctx);
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateServerCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Servers.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    auto additionalIncludes = QStringList() << QStringLiteral("../Thrift.h")
        << QStringLiteral("Types_io.h") << QStringLiteral("<Log.h>");
    sortIncludes(additionalIncludes);
    writeHeaderBody(ctx, QStringLiteral("Servers.h"), additionalIncludes);

    // First generate some helper functions

    ctx.m_out << "namespace {" << ln << ln;

    const auto & services = parser->services();
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
    Parser * parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("<QObject>");
    sortIncludes(additionalIncludes);

    const auto & services = parser->services();
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

void Generator::generateTestServerCpps(Parser * parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../../Http.h")
        << QStringLiteral("RandomDataGenerators.h")
        << QStringLiteral("<generated/Servers.h>")
        << QStringLiteral("<generated/Services.h>")
        << QStringLiteral("<QTcpServer>")
        << QStringLiteral("<QtTest/QtTest>");
    sortIncludes(additionalIncludes);

    const auto & enumerations = parser->enumerations();
    const auto & services = parser->services();
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
            generateTestServerHelperLambda(s, func, *parser, ctx);
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
                generateTestServerPrepareRequestExceptionResponse(*parser, e, ctx);
                generateTestServerHelperLambda(s, func, *parser, ctx, e.m_name);
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
                *parser, exceptionField, ctx);

            generateTestServerHelperLambda(
                s, func, *parser, ctx, exceptionField.m_name);

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
            generateTestServerHelperLambda(s, func, *parser, ctx);
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
                generateTestServerPrepareRequestExceptionResponse(*parser, e, ctx);
                generateTestServerHelperLambda(s, func, *parser, ctx, e.m_name);
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
                *parser, exceptionField, ctx);

            generateTestServerHelperLambda(
                s, func, *parser, ctx, exceptionField.m_name);

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
    Parser * parser, const QString & outPath)
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

    for(const auto & s: parser->structures())
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "();" << ln
            << ln;
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateTestRandomDataGeneratorsCpp(
    Parser * parser, const QString & outPath)
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

    for(const auto & s: parser->structures())
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "()" << ln
            << "{" << ln;

        ctx.m_out << "    " << s.m_name << " result;" << ln;

        for(const auto & f: s.m_fields) {
            generateGetRandomValueExpression(
                f,
                QStringLiteral("    result."),
                *parser,
                ctx.m_out);
        }

        ctx.m_out << "    return result;" << ln
            << "}" << ln << ln;
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateLocalDataClassDeclaration(
    OutputFileContext & ctx)
{
    ctx.m_out << "/**" << ln
        << " * @brief The EverCloudLocalData class contains several" << ln
        << " * data elements which are not synchronized with Evernote service"
        << ln
        << " * but which are nevertheless useful in applications using" << ln
        << " * QEverCloud to implement feature rich full sync Evernote clients."
        << ln
        << " * Values of this class' types are contained within QEverCloud"
        << ln
        << " * types corresponding to actual Evernote API types" << ln
        << " */" << ln;

    ctx.m_out << "class QEVERCLOUD_EXPORT EverCloudLocalData"
        << ": public Printable" << ln
        << "{" << ln
        << "    Q_GADGET" << ln
        << "public:" << ln
        << "    EverCloudLocalData();" << ln
        << "    virtual ~EverCloudLocalData() noexcept override;" << ln << ln;

    ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
        << ln << ln;

    ctx.m_out << "    bool operator==(const EverCloudLocalData & other) const;"
        << ln
        << "    bool operator!=(const EverCloudLocalData & other) const;"
        << ln;

    ctx.m_out << "    /**" << ln
        << "     * @brief id property can be used as a local unique identifier"
        << ln
        << "     * for any data item before it has been synchronized with"
        << ln
        << "     * Evernote and thus before it can be identified using its guid."
        << ln
        << "     *" << ln
        << "     * id property is generated automatically on EverCloudLocalData"
        << ln
        << "     * construction for convenience but can be overridden manually"
        << ln
        << "     */" << ln
        << "    QString id;" << ln << ln;

    ctx.m_out << "    /**" << ln
        << "     * @brief dirty property can be used to keep track which"
        << ln
        << "     * objects have been modified locally and thus need to be "
        << "synchronized" << ln
        << "     * with Evernote service" << ln
        << "     */" << ln
        << "    bool dirty = false;" << ln << ln;

    ctx.m_out << "    /**" << ln
        << "     * @brief local property can be used to keep track which"
        << ln
        << "     * data items are meant to be local only and thus never be "
        << "synchronized" << ln
        << "     * with Evernote service" << ln
        << "     */" << ln
        << "    bool local = false;" << ln << ln;

    ctx.m_out << "    /**" << ln
        << "     * @brief favorited property can be used to keep track which"
        << ln
        << "     * data items were favorited in the client. Unfortunately,"
        << ln
        << "     * Evernote has never provided a way to synchronize such"
        << ln
        << "     * property between different clients" << ln
        << "     */" << ln
        << "    bool favorited = false;" << ln << ln;

    ctx.m_out << "    /**" << ln
        << "     * @brief dict can be used for storage of any other auxiliary"
        << ln
        << "     * values associated with objects of QEverCloud types" << ln
        << "     */" << ln
        << "    QHash<QString, QVariant> dict;" << ln << ln;

    ctx.m_out << "    // Properties declaration for meta-object system" << ln
        << "    Q_PROPERTY(QString id MEMBER id USER true)" << ln
        << "    Q_PROPERTY(bool dirty MEMBER dirty)" << ln
        << "    Q_PROPERTY(bool local MEMBER local)" << ln
        << "    Q_PROPERTY(bool favorited MEMBER favorited)" << ln << ln
        << "    using Dict = QHash<QString, QVariant>;" << ln
        << "    Q_PROPERTY(Dict dict MEMBER dict)" << ln;

    ctx.m_out << "};" << ln;
}

void Generator::generateLocalDataClassDefinition(
    OutputFileContext & ctx)
{
    ctx.m_out << "EverCloudLocalData::EverCloudLocalData()" << ln
        << "{" << ln
        << "    id = QUuid::createUuid().toString();" << ln
        << "    // Remove curvy braces" << ln
        << "    id.remove(id.size() - 1, 1);" << ln
        << "    id.remove(0, 1);" << ln
        << "}" << ln << ln;

    ctx.m_out << "EverCloudLocalData::~EverCloudLocalData() noexcept" << ln
        << "{}" << ln << ln;

    ctx.m_out << "void EverCloudLocalData::print(QTextStream & strm) const"
        << ln
        << "{" << ln
        << "    strm << \"    localData.id = \" << id << \"\\n\"" << ln
        << "        << \"    localData.dirty = \" << (dirty ? \"true\" : \"false\")"
        << " << \"\\n\"" << ln
        << "        << \"    localData.local = \" << (local ? \"true\" : \"false\")"
        << " << \"\\n\"" << ln
        << "        << \"    localData.favorited = \" "
        << "<< (favorited ? \"true\" : \"false\")"
        << " << \"\\n\";" << ln << ln;

    ctx.m_out << "    if (!dict.isEmpty())" << ln
        << "    {" << ln
        << "        strm << \"    localData.dict:\" << \"\\n\";" << ln
        << "        QString valueStr;" << ln
        << "        for(const auto & it: toRange(dict)) {" << ln
        << "            strm << \"        [\" << it.key() << \"] = \";" << ln
        << "            valueStr.resize(0);" << ln
        << "            QDebug dbg(&valueStr);" << ln
        << "            dbg.noquote();" << ln
        << "            dbg.nospace();" << ln
        << "            dbg << it.value();" << ln
        << "            strm << valueStr << \"\\n\";" << ln
        << "        }" << ln
        << "    }" << ln;

    ctx.m_out << "}" << ln << ln;

    ctx.m_out << "bool EverCloudLocalData::operator==(" << ln
        << "    const EverCloudLocalData & other) const" << ln
        << "{" << ln
        << "    return id == other.id && dirty == other.dirty &&" << ln
        << "        local == other.local && favorited == other.favorited &&"
        << ln
        << "        dict == other.dict;" << ln
        << "}" << ln << ln
        << "bool EverCloudLocalData::operator!=(" << ln
        << "    const EverCloudLocalData & other) const" << ln
        << "{" << ln
        << "    return !operator==(other);" << ln
        << "}" << ln;
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

void Generator::generateSources(Parser * parser, const QString & outPath)
{
    if (parser->unions().count() > 0) {
        throw std::runtime_error("unions are not suported.");
    }

    m_baseTypes << QStringLiteral("bool") << QStringLiteral("byte")
        << QStringLiteral("i16") << QStringLiteral("i32")
        << QStringLiteral("i64") << QStringLiteral("double")
        << QStringLiteral("string") << QStringLiteral("binary");

    const auto & structures = parser->structures();
    for(const auto & s: structures) {
        m_allStructs << s.m_name;
    }

    const auto & exceptions = parser->exceptions();
    for(const auto & e: exceptions) {
        m_allExceptions << e.m_name;
    }

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations) {
        m_allEnums << e.m_name;
    }

    const auto & includes = parser->includes();
    for(const auto & include: includes) {
        QString s = include.m_name;
        s.replace(QStringLiteral("\""), QLatin1String(""));
        s.chop(QStringLiteral("thrift").length());
        m_includeList << s;
    }

    const auto & typedefs = parser->typedefs();
    for(const auto & t: typedefs)
    {
        auto casted = std::dynamic_pointer_cast<Parser::BaseType>(t.m_type);
        if (casted) {
            m_typedefMap[t.m_name] = casted->m_baseType;
        }
    }

    generateConstantsHeader(parser, outPath);
    generateConstantsCpp(parser, outPath);

    generateErrorsHeader(parser, outPath);
    generateErrorsCpp(parser, outPath);

    generateTypesIOHeader(parser, outPath);
    generateTypesHeader(parser, outPath);
    generateTypesCpp(parser, outPath);

    generateServicesHeader(parser, outPath);
    generateServicesCpp(parser, outPath);

    generateServerHeader(parser, outPath);
    generateServerCpp(parser, outPath);

    generateTestServerHeaders(parser, outPath);
    generateTestServerCpps(parser, outPath);

    generateTestRandomDataGeneratorsHeader(parser, outPath);
    generateTestRandomDataGeneratorsCpp(parser, outPath);
}

