/**
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Sergey Skoblikov, 2015-2019 Dmitry Ivanov
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

#include <algorithm>
#include <memory>

using namespace qevercloud;

namespace {

////////////////////////////////////////////////////////////////////////////////

static const char * disclaimer =
    "/**\n"
    " * Original work: Copyright (c) 2014 Sergey Skoblikov\n"
    " * Modified work: Copyright (c) 2015-2019 Dmitry Ivanov\n"
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
                << valueType << ">();" << endl;
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
                << valueType << ">();" << endl;
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
                << keyType << ", " << valueType << ">();" << endl;
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
    out << e.m_name << "();" << endl;

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
    out << "ThriftException(" << endl
        << "        ThriftException::Type::INTERNAL_ERROR," << endl
        << "        QStringLiteral(\"Internal error\"));" << endl;
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
    ctx.m_out << "namespace qevercloud {" << endl << endl;
}

void Generator::writeNamespaceEnd(QTextStream & out)
{
    out << "} // namespace qevercloud" << endl;
}

void Generator::writeEnumeration(
    OutputFileContext & ctx, const Parser::Enumeration & e) const
{
    if (!e.m_docComment.isEmpty()) {
        ctx.m_out << e.m_docComment << endl;
    }

    ctx.m_out << "enum class " << e.m_name << endl << "{" << endl;

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

        ctx.m_out << endl;
        ++i;
    }

    ctx.m_out << "};" << endl << endl;

    if (ctx.m_type == OutputFileType::Interface) {
        ctx.m_out << "#if QT_VERSION >= QT_VERSION_CHECK(5, 8, 0)" << endl
            << "Q_ENUM_NS(" << e.m_name << ")" << endl
            << "#endif" << endl << endl;
    }

    ctx.m_out << "inline uint qHash(" << e.m_name << " value)"
        << endl
        << "{" << endl
        << "    return static_cast<uint>(value);" << endl
        << "}" << endl << endl;
}

void Generator::writeEnumerationPrintDeclaration(
    QTextStream & out, const Parser::Enumeration & e,
    const char * printer) const
{
    out << "QEVERCLOUD_EXPORT " << printer << " & operator<<(" << endl
        << "    " << printer << " & out, const " << e.m_name << " value);"
        << endl << endl;
}

void Generator::writeEnumerationPrintDefinition(
    QTextStream & out, const Parser::Enumeration & e,
    const char * printer) const
{
    out << printer << " & operator<<(" << endl
        << "    " << printer << " & out, const "
        << e.m_name << " value)" << endl << "{" << endl
        << "    switch(value)" << endl
        << "    {" << endl;

    for(const auto & value: e.m_values) {
        out << "    case " << e.m_name << "::" << value.first << ":" << endl
            << "        out << \"" << e.m_name << "::" << value.first << "\";"
            << endl << "        break;" << endl;
    }

    out << "    default:" << endl
        << "        out << \"Unknown (\" << static_cast<qint64>(value) << \")\";"
        << endl << "        break;" << endl
        << "    }" << endl
        << "    return out;" << endl
        << "}" << endl << endl;
}

void Generator::writeStructPrintDefinition(
    QTextStream & out, const Parser::Structure & s,
    const Parser & parser) const
{
    out << "void " << s.m_name << "::print(QTextStream & strm) const"
        << endl
        << "{" << endl;

    out << "    strm << \"" << s.m_name << ": {\\n\";" << endl;

    const auto & exceptions = parser.exceptions();
    auto exceptionIt = std::find_if(
        exceptions.begin(),
        exceptions.end(),
        [&] (const Parser::Structure & e)
        {
            return e.m_name == s.m_name;
        });
    if (exceptionIt == exceptions.end()) {
        out << "    localData.print(strm);" << endl;
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
                out << endl;
            }

            out << "    if (" << f.m_name << ".isSet()) {" << endl
                << "        strm << \"    " << f.m_name << " = \"" << endl;

            if (mapType)
            {
                out << "            << \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << endl;
                out << "        for(const auto & it: toRange(" << f.m_name
                    << ".ref())) {" << endl;
                out << "            strm << \"        [\" << it.key() << \"] = \" "
                    << "<< it.value() << \"\\n\";" << endl;
                out << "        }" << endl;
                out << "        strm << \"    }\\n\";" << endl;
            }
            else if (setType)
            {
                out << "            << \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << endl;
                out << "        for(const auto & v: " << f.m_name
                    << ".ref()) {" << endl;
                out << "            strm << \"        \" << v << \"\\n\";" << endl;
                out << "        }" << endl;
                out << "        strm << \"    }\\n\";" << endl;
            }
            else if (listType)
            {
                out << "            << \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << endl;
                out << "        for(const auto & v: " << f.m_name
                    << ".ref()) {" << endl;
                out << "            strm << \"        \" << v << \"\\n\";" << endl;
                out << "        }" << endl;
                out << "        strm << \"    }\\n\";" << endl;
            }
            else
            {
                out << "            << "
                    << f.m_name << ".ref() << \"\\n\";" << endl;
            }

            out << "    }" << endl
                << "    else {" << endl
                << "        strm << \"    " << f.m_name << " is not set\\n\";"
                << endl
                << "    }" << endl << endl;
            previousOptional = true;
        }
        else
        {
            out << "    strm << \"    " << f.m_name << " = \"" << endl;

            if (mapType)
            {
                out << "        << \"QMap<"
                    << typeToStr(mapType->m_keyType, {}) << ", "
                    << typeToStr(mapType->m_valueType, {})
                    << "> {\";" << endl;
                out << "    for(const auto & it: toRange(" << f.m_name
                    << ")) {" << endl;
                out << "        strm << \"    [\" << it.key() << \"] = \" "
                    << "<< it.value() << \"\\n\";" << endl;
                out << "    }" << endl;
                out << "    strm << \"}\\n\";" << endl;
            }
            else if (setType)
            {
                out << "        << \"QSet<"
                    << typeToStr(setType->m_valueType, {}) << "> {\";" << endl;
                out << "    for(const auto & v: " << f.m_name
                    << ") {" << endl;
                out << "        strm << \"    \" << v << \"\\n\";" << endl;
                out << "    }" << endl;
                out << "    strm << \"}\\n\";" << endl;
            }
            else if (listType)
            {
                out << "        << \"QList<"
                    << typeToStr(listType->m_valueType, {}) << "> {\";" << endl;
                out << "    for(const auto & v: " << f.m_name
                    << ") {" << endl;
                out << "        strm << \"    \" << v << \"\\n\";" << endl;
                out << "    }" << endl;
                out << "    strm << \"}\\n\";" << endl;
            }
            else
            {
                out << "        << "
                    << f.m_name << " << \"\\n\";" << endl;
            }

            previousOptional = false;
        }
    }

    out << "    strm << \"}\\n\";" << endl
        << "}" << endl << endl;
}

void Generator::generateTestServerHelperClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & func: service.m_functions)
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << blockSeparator << endl << endl;

        QString funcName = capitalize(func.m_name);

        ctx.m_out << "class " << service.m_name << funcName
            << "TesterHelper: public QObject" << endl
            << "{" << endl
            << "    Q_OBJECT" << endl;

        auto responseType = typeToStr(func.m_type, func.m_name);
        ctx.m_out << "public:" << endl
            << "    using Executor = std::function<" << endl
            << "        " << responseType << "(" << endl;

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
            ctx.m_out << "            " << paramType << "," << endl;
        }

        ctx.m_out << "            IRequestContextPtr ctx)>;"
            << endl << endl;

        ctx.m_out << "public:" << endl
            << "    explicit " << service.m_name << funcName << "TesterHelper("
            << endl
            << "            Executor executor," << endl
            << "            QObject * parent = nullptr) :"
            << endl
            << "        QObject(parent)," << endl
            << "        m_executor(std::move(executor))" << endl
            << "    {}" << endl << endl;

        ctx.m_out << "Q_SIGNALS:" << endl
            << "    void " << func.m_name << "RequestReady(" << endl;
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        " << responseType << " value," << endl;
        }
        ctx.m_out << "        EverCloudExceptionDataPtr "
            << "exceptionData);" << endl << endl;

        ctx.m_out << "public Q_SLOTS:" << endl
            << "    void on" << funcName << "RequestReceived(" << endl;

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
                << "," << endl;
        }

        ctx.m_out << "        IRequestContextPtr ctx)" << endl
            << "    {" << endl;

        ctx.m_out << "        try" << endl
            << "        {" << endl;

        ctx.m_out << "            ";

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "auto v = ";
        }

        ctx.m_out << "m_executor(" << endl;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << endl;
        }

        ctx.m_out << "                ctx);" << endl << endl;

        ctx.m_out << "            Q_EMIT " << func.m_name << "RequestReady("
            << endl;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "                v," << endl;
        }

        ctx.m_out << "                "
            << "EverCloudExceptionDataPtr());" << endl
            << "        }" << endl;

        ctx.m_out << "        catch(const EverCloudException & e)" << endl
            << "        {" << endl;

        ctx.m_out << "            Q_EMIT " << func.m_name << "RequestReady("
            << endl;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "                {}," << endl;
        }

        ctx.m_out << "                "
            << "e.exceptionData());" << endl
            << "        }" << endl;

        ctx.m_out << "    }" << endl << endl;

        ctx.m_out << "private:" << endl
            << "    Executor m_executor;" << endl;

        ctx.m_out << "};" << endl << endl;
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

        ctx.m_out << blockSeparator << endl << endl;

        QString funcName = capitalize(func.m_name);

        ctx.m_out << "class " << service.m_name << funcName
            << "AsyncValueFetcher: public QObject" << endl
            << "{" << endl
            << "    Q_OBJECT" << endl
            << "public:" << endl
            << "    explicit " << service.m_name << funcName
            << "AsyncValueFetcher(QObject * parent = nullptr) :" << endl
            << "        QObject(parent)" << endl
            << "    {}" << endl << endl;

        auto responseType = typeToStr(func.m_type, {}, MethodType::TypeName);
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "    " << responseType << " m_value;" << endl;
        }

        ctx.m_out << "    EverCloudExceptionDataPtr m_exceptionData;"
            << endl << endl;

        ctx.m_out << "Q_SIGNALS:" << endl
            << "    void finished();" << endl << endl;

        ctx.m_out << "public Q_SLOTS:" << endl
            << "    void onFinished(" << endl
            << "        QVariant value," << endl
            << "        EverCloudExceptionDataPtr data," << endl
            << "        IRequestContextPtr ctx)" << endl
            << "    {" << endl;

        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        m_value = qvariant_cast<" << responseType
                << ">(value);" << endl;
        }
        else {
            ctx.m_out << "        Q_UNUSED(value)" << endl;
        }

        ctx.m_out << "        Q_UNUSED(ctx)" << endl;

        ctx.m_out << "        m_exceptionData = data;" << endl
            << "        Q_EMIT finished();" << endl
            << "    }" << endl
            << "};" << endl << endl;
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
                << "::" << e.m_values[index].first << ";" << endl;
        }
        else
        {
            ctx.m_out << getGenerateRandomValueFunction(actualParamTypeName)
                << ";" << endl;
        }
    }

    ctx.m_out << "    IRequestContextPtr ctx = newRequestContext(";
    if (hasAuthenticationToken) {
        ctx.m_out << endl
            << "        QStringLiteral(\"authenticationToken\")";
    }
    ctx.m_out << ");" << endl << endl;
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

        ctx.m_out << ";" << endl;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    response << "
                << getGenerateRandomValueFunction(actualValueType)
                << ";" << endl;
        }
        ctx.m_out << endl;

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

        ctx.m_out << ";" << endl;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    Q_UNUSED(response.insert("
                << getGenerateRandomValueFunction(actualValueType)
                << "))" << endl;
        }
        ctx.m_out << endl;
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

        ctx.m_out << ";" << endl;
        for(size_t i = 0; i < 3; ++i) {
            ctx.m_out << "    response["
                << getGenerateRandomValueFunction(actualKeyType)
                << "] = "
                << getGenerateRandomValueFunction(actualValueType)
                << ";" << endl;
        }
        ctx.m_out << endl;
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
                << "::" << e.m_values[index].first << ";" << endl << endl;
        }
        else
        {
            ctx.m_out << " = "
                << getGenerateRandomValueFunction(actualResponseTypeName)
                << ";" << endl << endl;
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
        ctx.m_out << endl;
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

    ctx.m_out << endl;
}

void Generator::generateTestServerHelperLambda(
    const Parser::Service & service,
    const Parser::Function & func,
    const Parser & parser,
    OutputFileContext & ctx,
    const QString & exceptionToThrow)
{
    ctx.m_out << "    " << service.m_name << capitalize(func.m_name)
        << "TesterHelper helper(" << endl
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
            << param.m_name << "Param," << endl;

        firstParam = false;
    }

    if (hasParams) {
        ctx.m_out << "             ";
    }

    auto returnTypeName = typeToStr(
        func.m_type,
        {},
        MethodType::TypeName);

    ctx.m_out << "IRequestContextPtr ctxParam) -> " << returnTypeName << endl;

    ctx.m_out << "        {" << endl;

    for(const auto & param: func.m_params)
    {
        if (param.m_name == QStringLiteral("authenticationToken")) {
            ctx.m_out << "            Q_ASSERT("
                << "ctx->authenticationToken() == "
                << "ctxParam->authenticationToken());" << endl;
            continue;
        }

        ctx.m_out << "            Q_ASSERT(" << param.m_name << " == "
            << param.m_name << "Param);" << endl;
    }

    if (!exceptionToThrow.isEmpty()) {
        ctx.m_out << "            throw " << exceptionToThrow << ";" << endl
            << "        });" << endl << endl;
        return;
    }

    ctx.m_out << "            return";

    auto funcVoidType = std::dynamic_pointer_cast<Parser::VoidType>(func.m_type);
    if (!funcVoidType) {
        ctx.m_out << " response";
    }

    ctx.m_out << ";" << endl
        << "        });" << endl << endl;
}

void Generator::generateTestServerSocketSetup(
    const Parser::Service & service,
    const Parser::Function & func,
    OutputFileContext & ctx)
{
    auto funcName = capitalize(func.m_name);

    ctx.m_out << "    " << service.m_name << "Server server;" << endl
        << "    QObject::connect(" << endl
        << "        &server," << endl
        << "        &" << service.m_name << "Server::" << func.m_name
        << "Request," << endl
        << "        &helper," << endl
        << "        &" << service.m_name << funcName
        << "TesterHelper::on" << funcName << "RequestReceived);"
        << endl;

    ctx.m_out << "    QObject::connect(" << endl
        << "        &helper," << endl
        << "        &" << service.m_name << funcName << "TesterHelper::"
        << func.m_name << "RequestReady," << endl
        << "        &server," << endl
        << "        &" << service.m_name << "Server::on" << funcName
        << "RequestReady);" << endl << endl;

    ctx.m_out << "    QTcpServer tcpServer;" << endl
        << "    QVERIFY(tcpServer.listen(QHostAddress::LocalHost));"
        << endl
        << "    quint16 port = tcpServer.serverPort();" << endl
        << endl;

    ctx.m_out << "    QTcpSocket * pSocket = nullptr;" << endl
        << "    QObject::connect(" << endl
        << "        &tcpServer," << endl
        << "        &QTcpServer::newConnection," << endl
        << "        [&] {" << endl
        << "            pSocket = tcpServer.nextPendingConnection();"
        << endl
        << "            Q_ASSERT(pSocket);" << endl
        << "            QObject::connect(" << endl
        << "                pSocket," << endl
        << "                &QAbstractSocket::disconnected," << endl
        << "                pSocket," << endl
        << "                &QAbstractSocket::deleteLater);" << endl
        << "            if (!pSocket->waitForConnected()) {" << endl
        << "                QFAIL(\"Failed to establish connection\");"
        << endl
        << "            }" << endl << endl
        << "            QByteArray requestData = "
        << "readThriftRequestFromSocket(*pSocket);" << endl
        << "            server.onRequest(requestData);" << endl
        << "        });" << endl << endl;

    ctx.m_out << "    QObject::connect(" << endl
        << "        &server," << endl
        << "        &" << service.m_name << "Server::" << func.m_name
        << "RequestReady," << endl
        << "        [&] (QByteArray responseData)" << endl
        << "        {" << endl
        << "            QByteArray buffer;" << endl
        << "            buffer.append(\"HTTP/1.1 200 OK\\r\\n\");"
        << endl
        << "            buffer.append(\"Content-Length: \");" << endl
        << "            buffer.append(QString::number("
        << "responseData.size()).toUtf8());" << endl
        << "            buffer.append(\"\\r\\n\");" << endl
        << "            buffer.append(\"Content-Type: "
        << "application/x-thrift\\r\\n\\r\\n\");" << endl
        << "            buffer.append(responseData);" << endl << endl
        << "            if (!writeBufferToSocket(buffer, "
        << "*pSocket)) {" << endl
        << "                QFAIL(\"Failed to write response to socket\");"
        << endl
        << "            }" << endl
        << "        });" << endl << endl;
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

    ctx.m_out << "    auto " << serviceName
        << " = new" << service.m_name << "(" << endl
        << "        QStringLiteral(\"http://127.0.0.1:\") + "
        << "QString::number(port)," << endl
        << "        nullptr," << endl
        << "        nullptr," << endl
        << "        nullRetryPolicy());" << endl;

    QString indent = QStringLiteral("    ");
    if (!exceptionTypeToCatch.isEmpty())
    {
        ctx.m_out << indent << "bool caughtException = false;" << endl;

        ctx.m_out << indent << "try" << endl
            << indent << "{" << endl;

        indent += indent;
    }

    if (callKind == ServiceCallKind::Sync)
    {
        ctx.m_out << indent;
        if (funcReturnTypeName != QStringLiteral("void")) {
            ctx.m_out << funcReturnTypeName << " res = ";
        }

        ctx.m_out << serviceName << "->" << func.m_name << "(" << endl;
    }
    else if (callKind == ServiceCallKind::Async)
    {
        ctx.m_out << indent << "AsyncResult * result = "
            << serviceName << "->" << func.m_name << "Async(" << endl;
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

        ctx.m_out << indent << "    " << param.m_name << "," << endl;
    }

    ctx.m_out << indent << "    ctx);" << endl;

    if (callKind == ServiceCallKind::Async)
    {
        auto funcName = capitalize(func.m_name);

        ctx.m_out << endl << indent << service.m_name << funcName
            << "AsyncValueFetcher valueFetcher;" << endl;

        ctx.m_out << indent << "QObject::connect(" << endl
            << indent << "    result," << endl
            << indent << "    &AsyncResult::finished," << endl
            << indent << "    &valueFetcher," << endl
            << indent << "    &"
            << service.m_name << funcName << "AsyncValueFetcher::onFinished);"
            << endl << endl;

        ctx.m_out << indent << "QEventLoop loop;" << endl
            << indent << "QObject::connect(" << endl
            << indent << "    &valueFetcher," << endl
            << indent << "    &"
            << service.m_name << funcName << "AsyncValueFetcher::finished,"
            << endl
            << indent << "    &loop," << endl
            << indent << "    &QEventLoop::quit);" << endl << endl
            << indent << "loop.exec();" << endl << endl;

        if (exceptionTypeToCatch.isEmpty())
        {
            if (funcReturnTypeName != QStringLiteral("void")) {
                ctx.m_out << indent << "QVERIFY(valueFetcher.m_value == response);"
                    << endl;
            }

            ctx.m_out << indent << "QVERIFY(valueFetcher.m_exceptionData.get() == nullptr);"
                << endl;
        }
        else
        {
            ctx.m_out << indent << "QVERIFY(valueFetcher.m_exceptionData.get() != nullptr);"
                << endl;
            ctx.m_out << indent << "valueFetcher.m_exceptionData->throwException();"
                << endl;
        }
    }

    if (!exceptionTypeToCatch.isEmpty())
    {
        if ((callKind == ServiceCallKind::Sync) &&
            (funcReturnTypeName != QStringLiteral("void")))
        {
            ctx.m_out << indent << "Q_UNUSED(res)" << endl;
        }

        ctx.m_out << "    }" << endl
            << "    catch(const " << exceptionTypeToCatch << " & e)" << endl
            << "    {" << endl
            << "        caughtException = true;" << endl
            << "        QVERIFY(e == " << exceptionNameToCompare << ");" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    QVERIFY(caughtException);" << endl;
        return;
    }

    if ((callKind == ServiceCallKind::Sync) &&
        (funcReturnTypeName != QStringLiteral("void")))
    {
        ctx.m_out << "    QVERIFY(res == response);" << endl;
    }
}

void Generator::writeHeaderHeader(
    OutputFileContext & ctx, const QString & fileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind)
{
    ctx.m_out << disclaimer << endl;

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());
    ctx.m_out << "#ifndef " << guard << endl;
    ctx.m_out << "#define " << guard << endl;
    ctx.m_out << endl;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include \"../Export.h\"" << endl;
        ctx.m_out << endl;
    }

    for(const auto & include: qAsConst(additionalIncludes))
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            ctx.m_out << "#include " << include << endl;
        }
        else {
            ctx.m_out << "#include \"" << include << "\"" << endl;
        }
    }

    if (!additionalIncludes.isEmpty()) {
        ctx.m_out << endl;
    }

    writeNamespaceBegin(ctx);
}

void Generator::writeHeaderBody(
    OutputFileContext & ctx, const QString & headerFileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind)
{
    ctx.m_out << disclaimer << endl;

    if (headerKind == HeaderKind::Public) {
        ctx.m_out << "#include <generated/" << headerFileName << ">" << endl;
    }
    else {
        ctx.m_out << "#include \"" << headerFileName << "\"" << endl;
    }

    if (headerKind == HeaderKind::Test) {
        ctx.m_out << "#include \"../../Impl.h\"" << endl;
    }
    else {
        ctx.m_out << "#include \"../Impl.h\"" << endl;
    }

    for(const auto & include: additionalIncludes)
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            ctx.m_out << "#include " << include << endl;
        }
        else {
            ctx.m_out << "#include \"" << include << "\"" << endl;
        }
    }

    ctx.m_out << endl;
    writeNamespaceBegin(ctx);
}

void Generator::writeHeaderFooter(
    QTextStream & out, const QString & fileName,
    const QStringList & extraLinesInsideNamespace,
    const QStringList & extraLinesOutsideNamespace)
{
    for(const auto & line: extraLinesInsideNamespace) {
        out << line << endl;
    }

    if (!extraLinesInsideNamespace.empty()) {
        out << endl;
    }

    writeNamespaceEnd(out);

    if (!extraLinesOutsideNamespace.empty()) {
        out << endl;
    }

    for(const auto & line: extraLinesOutsideNamespace) {
        out << line << endl;
    }

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());

    out << endl;
    out << "#endif // " << guard << endl;
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
            default: result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
                result = QLatin1Literal("");
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
        default: result = QLatin1Literal("");
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
                        result = QLatin1Literal("");
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
                        result = QLatin1Literal("");
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
            result = QLatin1Literal("");
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
            result = QLatin1Literal("");
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
            result = QLatin1Literal("");
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
        strm << endl;
        for(const auto & v: qAsConst(listValue->m_values)) {
            strm << offset << "<< "
                << valueToStr(v, std::shared_ptr<Parser::Type>(nullptr),
                              identifier, nextOffset)
                << endl;
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

    ctx.m_out << blockSeparator << endl << endl;

    const auto & constants = parser->constants();
    for(const auto & c: constants)
    {
        if (c.m_fileName != fileName) {
            ctx.m_out << "// " << c.m_fileName << endl;
        }

        if (!c.m_docComment.isEmpty()) {
            ctx.m_out << c.m_docComment << endl;
        }

        ctx.m_out << "QEVERCLOUD_EXPORT extern const "
            << typeToStr(c.m_type, c.m_name)
            << " " << c.m_name << ";" << endl << endl;
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

    ctx.m_out << blockSeparator << endl << endl;

    const auto & constants = parser->constants();
    for(const auto & c: constants)
    {
        if (c.m_fileName != fileName) {
            ctx.m_out << "// " << c.m_fileName << endl << endl;
        }

        if (!c.m_value) {
            throw std::runtime_error(
                QString::fromUtf8("Constant without a value: %1")
                .arg(c.m_name).toStdString());
        }

        ctx.m_out << "const " << typeToStr(c.m_type, c.m_name) << " "
            << c.m_name << " = "
            << valueToStr(c.m_value, c.m_type, c.m_name, QStringLiteral("    "))
            << ";" << endl;
    }

    ctx.m_out << endl;
    writeNamespaceEnd(ctx.m_out);
}

QString Generator::fieldDeclarationToStr(const Parser::Field & field)
{
    QString s = typeToStr(field.m_type, field.m_name);
    if (field.m_required == Parser::Field::RequiredFlag::Optional) {
        s = QStringLiteral("Optional<") + s + QStringLiteral(">");
    }

    s += QStringLiteral(" ") + field.m_name;
    if (field.m_initializer) {
        s += QStringLiteral(" = ") +
            valueToStr(field.m_initializer, field.m_type, field.m_name);
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
        QString ident = QLatin1Literal("");

        bool isOptional =
            (field.m_required == Parser::Field::RequiredFlag::Optional);
        if (isOptional) {
            ident = QStringLiteral("    ");
            out << "    if (" << fieldPrefix
                << field.m_name << ".isSet()) {" << endl;
        }

        out << ident << "    writer.writeFieldBegin(" << endl
            << ident << "        QStringLiteral(\""
            << field.m_name << "\")," << endl
            << ident << "        " << typeToStr(
                field.m_type, identPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << "," << endl
            << ident << "        " << field.m_id << ");" << endl << endl;

        QString fieldMoniker;
        {
            QTextStream strm(&fieldMoniker);
            strm << fieldPrefix << field.m_name
                << (isOptional ? QStringLiteral(".ref()") : QLatin1Literal(""));
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
                << ", " << fieldMoniker << ".length());" << endl;

            out << ident
                << "    for(const auto & value: qAsConst("
                << fieldMoniker << ")) {" << endl;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << "        " << writeMethod << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << ");" << endl;

            out << ident << "    }" << endl;
            out << ident << "    writer.writeListEnd();" << endl << endl;
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
                << ".count());" << endl;

            out << ident << "    for(const auto & value: qAsConst("
                << fieldMoniker << ")) {" << endl;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << "        " << writeMethod
                << "value"
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << ");" << endl;

            out << ident << "    }" << endl;
            out << ident << "    writer.writeSetEnd();" << endl << endl;
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
                << ".size());" << endl;

            out << ident << "    for(const auto & it: "
                << "toRange(" << fieldMoniker
                << ")) {" << endl;

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
                    : QLatin1Literal(""))
                << ");" << endl;
            out << ident << "        " << valueWriteMethod << "it.value()"
                << (valueWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << ");" << endl;

            out << ident << "    }" << endl;
            out << ident << "    writer.writeMapEnd();" << endl << endl;
        }
        else
        {
            out << ident << "    " << writeMethod << fieldMoniker
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << ");" << endl;
        }

        out << ident << "    writer.writeFieldEnd();" << endl;
        if (isOptional) {
            out << "    }" << endl;
        }
        out << endl;
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
        << " v;" << endl;

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

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType elemType;" << endl;
        out << indent << "reader.readListBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if (elemType != " << valueThriftType
            << ") {" << endl << indent << "    throw ThriftException("
            << endl << indent << "        ThriftException::Type::"
            << "INVALID_DATA," << endl << indent
            << "        QStringLiteral(\"Incorrect list type ("
            << identPrefix + field.m_name << ")\"));" << endl
            << indent << "}" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {"
            << endl;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.append(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "reader.readListEnd();" << endl;
    }
    else if (readMethod.contains(QStringLiteral("readSetBegin")))
    {
        auto valueType =
            std::dynamic_pointer_cast<Parser::SetType>(field.m_type)->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType elemType;" << endl;
        out << indent << "reader.readSetBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if (elemType != " << valueThriftType
            << ") {" << endl
            << indent << "    throw ThriftException(" << endl
            << indent << "        ThriftException::Type::INVALID_DATA," << endl
            << indent << "        QStringLiteral(\"Incorrect set type ("
            << identPrefix + field.m_name << ")\"));" << endl
            << indent << "}" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.insert(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "reader.readSetEnd();" << endl;
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

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType keyType;" << endl;
        out << indent << "ThriftFieldType elemType;" << endl;
        out << indent << "reader.readMapBegin(keyType, elemType, size);" << endl;
        out << indent << "if (keyType != " << keyThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map key type ("
            << identPrefix << field.m_name << ")\"));" << endl;
        out << indent << "if (elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect map value type ("
            << identPrefix + field.m_name << ")\"));" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    "
            << typeToStr(
                keyType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " key;" << endl;
        out << indent << "    " << keyReadMethod << "key);" << endl;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " value;" << endl;
        out << indent << "    " << valueReadMethod << "value);" << endl;
        out << indent << "    v[key] = value;" << endl;
        out << indent << "}" << endl;
        out << indent << "reader.readMapEnd();" << endl;
    }
    else
    {
        out << indent << readMethod << "v);" << endl;
    }

    out << indent << fieldParent << field.m_name << " = v;" << endl;
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

    const auto & enumerations = parser->enumerations();
    int enumerationsCount = enumerations.size();
    int i = 0;
    for(const auto & e: enumerations)
    {
        writeEnumeration(ctx, e);
        ctx.m_out << blockSeparator << endl << endl;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QTextStream");
        ctx.m_out << blockSeparator << endl << endl;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QDebug");
        if (i != (enumerationsCount - 1)) {
            ctx.m_out << blockSeparator << endl << endl;
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

    ctx.m_out << blockSeparator << endl << endl;

    const auto & enumerations = parser->enumerations();
    int enumerationsCount = enumerations.size();
    int i = 0;
    for(const auto & e: enumerations)
    {
        writeEnumerationPrintDefinition(ctx.m_out, e, "QTextStream");
        ctx.m_out << blockSeparator << endl << endl;

        writeEnumerationPrintDefinition(ctx.m_out, e, "QDebug");
        if (i != (enumerationsCount - 1)) {
            ctx.m_out << blockSeparator << endl << endl;
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

    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    QList<const QList<Parser::Structure>*> lists;
    lists.reserve(2);
    lists << &parser->structures();
    lists << &parser->exceptions();

    for(const auto & pList: qAsConst(lists))
    {
        for(const auto & s: *pList) {
            ctx.m_out << "void write" << s.m_name
                << "(ThriftBinaryBufferWriter & writer, const "
                << s.m_name << " & s);" << endl;
            ctx.m_out << "void read" << s.m_name
                << "(ThriftBinaryBufferReader & reader, "
                << s.m_name << " & s);" << endl;
        }
    }
    ctx.m_out << endl;

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations) {
        ctx.m_out << "void readEnum" << e.m_name
            << "(ThriftBinaryBufferReader & reader, "
            << e.m_name << " & e);" << endl;
    }
    ctx.m_out << endl << "/** @endcond */" << endl;

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
            ctx.m_out << t.m_docComment << endl;
        }

        ctx.m_out << "using " << t.m_name << " = "
            << typeToStr(t.m_type, t.m_name) << ";" << endl << endl;
    }
    ctx.m_out << endl;

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
    ctx.m_out << endl;

    for(const auto & s: qAsConst(ordered))
    {
        if (!s.m_docComment.isEmpty()) {
            ctx.m_out << s.m_docComment << endl;
        }
        else {
            ctx.m_out << "/** NO DOC COMMENT ID FOUND */" << endl;
        }

        if (exceptions.contains(s.m_name))
        {
            ctx.m_out << "class QEVERCLOUD_EXPORT " << s.m_name
                << ": public EvernoteException, public Printable"
                << endl << "{" << endl
                << "    Q_GADGET" << endl
                << "public:" << endl;

            for(const auto & f : s.m_fields) {
                ctx.m_out << "    " << fieldDeclarationToStr(f) << ";" << endl;
            }

            ctx.m_out << endl;
            ctx.m_out << "    " << s.m_name
                << "();" << endl;
            ctx.m_out << "    virtual ~" << s.m_name
                << "() noexcept override;" << endl;

            if (!s.m_fields.isEmpty()) {
                ctx.m_out << endl;
                ctx.m_out << "    " << s.m_name
                    << "(const " << s.m_name
                    << " & other);" << endl;
            }

            ctx.m_out << "    const char * what() const noexcept override;"
                << endl;
            ctx.m_out << "    virtual EverCloudExceptionDataPtr "
                << "exceptionData() const override;" << endl << endl;
            ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
                << endl;
        }
        else
        {
            ctx.m_out << "struct QEVERCLOUD_EXPORT "
                << s.m_name << ": public Printable" << endl
                << "{" << endl
                << "private:" << endl
                << "    Q_GADGET" << endl
                << "public:" << endl;

            ctx.m_out << "    /**" << endl
                << "     * See the declaration of EverCloudLocalData for details"
                << endl
                << "     */" << endl
                << "    EverCloudLocalData localData;" << endl << endl;

            for(const auto & f: qAsConst(s.m_fields))
            {
                if (s.m_fieldComments.contains(f.m_name))
                {
                    auto lines =
                        s.m_fieldComments[f.m_name].split(QStringLiteral("\n"));
                    for(const auto & line: lines) {
                        ctx.m_out << "    " << line << endl;
                    }
                }
                else
                {
                    ctx.m_out << "    /** NOT DOCUMENTED */" << endl;
                }

                ctx.m_out << "    " << fieldDeclarationToStr(f) << ";" << endl;
            }

            ctx.m_out << endl;
            ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
                << endl;
        }

        ctx.m_out << endl;
        ctx.m_out << QString::fromUtf8(
            "    bool operator==(const %1 & other) const").arg(s.m_name) << endl;
        ctx.m_out << "    {" << endl;

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
                    << endl;
            }
            else {
                ctx.m_out << QString::fromUtf8("(%1 == other.%1)").arg(f.m_name)
                    << endl;
            }
        }

        ctx.m_out << "        ;" << endl << "    }" << endl << endl;
        ctx.m_out << QString::fromUtf8(
            "    bool operator!=(const %1 & other) const").arg(s.m_name)
            << endl;
        ctx.m_out << "    {" << endl;
        ctx.m_out << "        return !(*this == other);" << endl;
        ctx.m_out << "    }" << endl;

        ctx.m_out << endl;

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
	        << it.key() << ";" << endl;
        }

        if (!typeDefs.isEmpty()) {
            ctx.m_out << endl;
        }

        if (!exceptions.contains(s.m_name)) {
            ctx.m_out << "    Q_PROPERTY(EverCloudLocalData localData MEMBER "
                << "localData)" << endl;
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
                << " " << f.m_name << " MEMBER " << f.m_name << ")" << endl;
        }

        ctx.m_out << "};" << endl << endl;
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

    ctx.m_out << blockSeparator << endl << endl;
    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations)
    {
        ctx.m_out <<  "void readEnum" << e.m_name
            << "(" << endl << "    ThriftBinaryBufferReader & reader," << endl
            << "    " << e.m_name << " & e)" << endl << "{" << endl;

        ctx.m_out << "    qint32 i;" << endl;
        ctx.m_out << "    reader.readI32(i);" << endl;
        ctx.m_out << "    switch(i) {" << endl;

        for(const auto & v : e.m_values) {
            QString value = e.m_name + QStringLiteral("::") + v.first;
            ctx.m_out << "    case static_cast<int>("
                << value << "): e = " << value
                << "; break;" << endl;
        }

        ctx.m_out << "    default: throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect value for enum "
            << e.m_name << "\"));" << endl;
        ctx.m_out << "    }" << endl;
        ctx.m_out << "}" << endl << endl;
    }

    QSet<QString> exceptions;
    for(const auto & e: parser->exceptions()) {
        exceptions.insert(e.m_name);
    }

    auto structsAndExceptions = parser->structures();
    structsAndExceptions << parser->exceptions();

    generateLocalDataClassDefinition(ctx);
    ctx.m_out << endl;

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        if (exceptions.contains(s.m_name))
        {
            ctx.m_out << s.m_name << "::" << s.m_name
                << "() {}" << endl;
            ctx.m_out << s.m_name << "::~" << s.m_name
                << "() noexcept {}" << endl;

            if (!s.m_fields.isEmpty())
            {
                ctx.m_out << s.m_name << "::" << s.m_name
                    << "(const " << s.m_name
                    << "& other) : EvernoteException(other)" << endl;
                ctx.m_out << "{" << endl;
                for(const auto & f : s.m_fields) {
                    ctx.m_out << "   " << f.m_name
                        << " = other." << f.m_name
                        << ";" << endl;
                }
                ctx.m_out << "}" << endl;
            }
        }

        ctx.m_out << "void write" << s.m_name
            << "(" << endl << "    ThriftBinaryBufferWriter & writer," << endl
            << "    const "
            << s.m_name << " & s)" << endl << "{" << endl;

        ctx.m_out << "    writer.writeStructBegin(QStringLiteral(\""
            << s.m_name  << "\"));" << endl;

        writeThriftWriteFields(
            ctx.m_out, s.m_fields, s.m_name, QStringLiteral("s."));

        ctx.m_out << "    writer.writeFieldStop();" << endl;
        ctx.m_out << "    writer.writeStructEnd();" << endl;
        ctx.m_out << "}" << endl << endl;

        ctx.m_out << "void read" << s.m_name
            << "(" << endl << "    ThriftBinaryBufferReader & reader," << endl
            << "    " << s.m_name << " & s)" << endl << "{" << endl;
        ctx.m_out << "    QString fname;" << endl;
        ctx.m_out << "    ThriftFieldType fieldType;" << endl;
        ctx.m_out << "    qint16 fieldId;" << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    bool " << field.m_name
                    << "_isset = false;" << endl;
            }
        }

        ctx.m_out << "    reader.readStructBegin(fname);" << endl;
        ctx.m_out << "    while(true)" << endl
            << "    {" << endl;
        ctx.m_out << "        reader.readFieldBegin(fname, fieldType, fieldId);"
            << endl;
        ctx.m_out << "        if (fieldType == "
            << "ThriftFieldType::T_STOP) break;" << endl;

        for(const auto & field : s.m_fields)
        {
            bool isOptional =
                (field.m_required == Parser::Field::RequiredFlag::Optional);
            ctx.m_out << "        if (fieldId == " << field.m_id
                << ") {" << endl;
            ctx.m_out << "            if (fieldType == "
                << typeToStr(
                    field.m_type, s.m_name + QStringLiteral(".") + field.m_name,
                    MethodType::ThriftFieldType)
                << ") {" << endl;

            if (!isOptional) {
                ctx.m_out << "                " << field.m_name
                    << "_isset = true;" << endl;
            }

            writeThriftReadField(
                ctx.m_out, field, s.m_name + QStringLiteral("."),
                QStringLiteral("s."));

            ctx.m_out << "            }" << endl
                << "            else {" << endl;
            ctx.m_out << "                reader.skip(fieldType);" << endl;
            ctx.m_out << "            }" << endl;
            ctx.m_out << "        }" << endl
                << "        else" << endl;
        }

        ctx.m_out << "        {" << endl;
        ctx.m_out << "            reader.skip(fieldType);" << endl;
        ctx.m_out << "        }" << endl;
        ctx.m_out << "        reader.readFieldEnd();" << endl;
        ctx.m_out << "    }" << endl;
        ctx.m_out << "    reader.readStructEnd();" << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    if (!" << field.m_name
                    << "_isset) throw ThriftException("
                    << "ThriftException::Type::INVALID_DATA, "
                    << "QStringLiteral(\""
                    << s.m_name << "." << field.m_name
                    << " has no value\"));"
                    << endl;
            }
        }

        ctx.m_out << "}" << endl << endl;

        writeStructPrintDefinition(ctx.m_out, s, *parser);
        ctx.m_out << blockSeparator << endl << endl;
    }

    ctx.m_out << "/** @endcond */" << endl << endl;

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

        ctx.m_out << blockSeparator << endl << endl;

        if (!s.m_docComment.isEmpty()) {
            ctx.m_out << s.m_docComment << endl;
        }

        ctx.m_out << "class QEVERCLOUD_EXPORT I" << s.m_name
            << ": public QObject" << endl << "{" << endl;
        ctx.m_out << "    Q_OBJECT" << endl;
        ctx.m_out << "    Q_DISABLE_COPY(I" << s.m_name << ")" << endl;
        ctx.m_out << "protected:"<< endl;
        ctx.m_out << "    I" << s.m_name << "(QObject * parent) :" << endl;
        ctx.m_out << "        QObject(parent)" << endl;
        ctx.m_out << "    {}" << endl << endl;
        ctx.m_out << "public:" << endl;
        ctx.m_out << "    virtual QString " << decapitalize(s.m_name)
            << "Url() const = 0;" << endl;
        ctx.m_out << "    virtual void set" << s.m_name << "Url(QString url) = 0;"
            << endl << endl;

        for(const auto & func: qAsConst(s.m_functions))
        {
            if (func.m_isOneway) {
                throw std::runtime_error("oneway functions are not supported");
            }

            if (!func.m_docComment.isEmpty())
            {
                QStringList lines = func.m_docComment.split(
                    QChar::fromLatin1('\n'));

                for(const auto & line: lines) {
                    ctx.m_out << "    " << line << endl;
                }
            }

            ctx.m_out << "    virtual " << typeToStr(func.m_type, func.m_name) << " "
                 << func.m_name << "(";
            if (!func.m_params.isEmpty()) {
                ctx.m_out << endl;
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

                ctx.m_out << "," << endl;
            }

            if (!func.m_params.isEmpty()) {
                ctx.m_out << "        ";
            }
            ctx.m_out << "IRequestContextPtr ctx = {}";
            ctx.m_out << ") = 0;" << endl << endl;

            ctx.m_out << "    /** Asynchronous version of @link " << func.m_name
                << " @endlink */" << endl;
            ctx.m_out << "    virtual AsyncResult * " << func.m_name << "Async(" << endl;
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

                ctx.m_out << "," << endl;
            }

            ctx.m_out << "        IRequestContextPtr ctx = {}";
            ctx.m_out << ") = 0;" << endl << endl;
        }

        ctx.m_out << "};" << endl << endl;
        ctx.m_out << "using I" << s.m_name << "Ptr = std::shared_ptr<I"
            << s.m_name << ">;" << endl << endl;
    }

    if (!services.isEmpty())
    {
        ctx.m_out << blockSeparator << endl << endl;

        for(const auto & s: services)
        {
            ctx.m_out << "QEVERCLOUD_EXPORT I" << s.m_name << " * new"
                << s.m_name << "(" << endl;

            ctx.m_out << "    QString " << decapitalize(s.m_name)
                << "Url = {}," << endl
                << "    IRequestContextPtr ctx = {}," << endl
                << "    QObject * parent = nullptr," << endl
                << "    IRetryPolicyPtr retryPolicy = {});" << endl;
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
        ctx.m_out << blockSeparator << endl << endl;
        generateServiceClassDeclaration(s, ServiceClassType::NonDurable, ctx);
        generateServiceClassDefinition(s, ctx);
    }

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << endl << endl;
        generateServiceClassDeclaration(s, ServiceClassType::Durable, ctx);
    }

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << endl << endl;
        generateDurableServiceClassDefinition(s, ctx);
    }

    ctx.m_out << blockSeparator << endl << endl;

    for(const auto & s: services)
    {
        ctx.m_out << "I" << s.m_name << " * new" << s.m_name << "(" << endl;

        auto serviceName = decapitalize(s.m_name);

        ctx.m_out << "    QString " << serviceName << "Url," << endl
            << "    IRequestContextPtr ctx," << endl
            << "    QObject * parent," << endl
            << "    IRetryPolicyPtr retryPolicy)" << endl
            << "{" << endl
            << "    if (ctx && ctx->maxRequestRetryCount() == 0)" << endl
            << "    {" << endl
            << "        return new " << s.m_name << "(" << serviceName
            << "Url, ctx);" << endl
            << "    }" << endl
            << "    else" << endl
            << "    {" << endl
            << "        if (!retryPolicy) {" << endl
            << "            retryPolicy = newRetryPolicy();" << endl
            << "        }" << endl << endl
            << "        return new Durable" << s.m_name << "(" << endl
            << "            std::make_shared<" << s.m_name << ">("
            << serviceName << "Url, ctx)," << endl
            << "            ctx," << endl
            << "            retryPolicy," << endl
            << "            parent);" << endl
            << "    }" << endl
            << "}" << endl
            << endl;
    }

    writeNamespaceEnd(ctx.m_out);

    ctx.m_out << endl;
    ctx.m_out << "#include <Services.moc>" << endl;
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

    ctx.m_out << "namespace {" << endl << endl;

    const auto & services = parser->services();
    for(const auto & s: services) {
        generateServerHelperFunctions(s, ctx);
    }

    ctx.m_out << "} // namespace" << endl << endl;

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
        << QStringLiteral("../SocketHelpers.h") << QStringLiteral("<QObject>");
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

        ctx.m_out << blockSeparator << endl << endl;

        ctx.m_out << "class " << s.m_name << "Tester: public QObject" << endl
            << "{" << endl
            << "    Q_OBJECT" << endl
            << "public:" << endl
            << "    explicit " << s.m_name << "Tester(QObject * parent = nullptr);"
            << endl << endl
            << "private Q_SLOTS:" << endl;

        for(const auto & func: s.m_functions)
        {
            if (func.m_isOneway) {
                throw std::runtime_error("oneway functions are not supported");
            }

            auto funcName = capitalize(func.m_name);

            // Tests for synchronous methods

            ctx.m_out << "    void shouldExecute" << funcName << "();" << endl;

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "    void shouldDeliver" << exceptionTypeName
                    << "In" << funcName << "();" << endl;
            }

            ctx.m_out << "    void shouldDeliverThriftExceptionIn" << funcName
                << "();" << endl;

            // Tests for asynchronous methods

            ctx.m_out << "    void shouldExecute" << funcName << "Async();"
                << endl;

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "    void shouldDeliver" << exceptionTypeName
                    << "In" << funcName << "Async();" << endl;
            }

            ctx.m_out << "    void shouldDeliverThriftExceptionIn" << funcName
                << "Async();" << endl;
        }

        ctx.m_out << "};" << endl << endl;

        writeHeaderFooter(ctx.m_out, fileName);
    }
}

void Generator::generateTestServerCpps(Parser * parser, const QString & outPath)
{
    auto additionalIncludes = QStringList()
        << QStringLiteral("../SocketHelpers.h")
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

        ctx.m_out << blockSeparator << endl << endl;

        ctx.m_out << s.m_name << "Tester::" << s.m_name << "Tester"
            << "(QObject * parent) :" << endl
            << "    QObject(parent)" << endl
            << "{}" << endl << endl;

        generateTestServerHelperClassDefinition(s, ctx);
        generateTestServerAsyncValueFetcherClassDefinition(s, ctx);

        for(const auto & func: s.m_functions)
        {
            ctx.m_out << blockSeparator << endl << endl;

            auto funcName = capitalize(func.m_name);

            // Should deliver request and response for successful synchronous
            // calls

            ctx.m_out << "void " << s.m_name << "Tester::shouldExecute"
                << funcName << "()" << endl;

            ctx.m_out << "{" << endl;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestResponse(func, enumerations, ctx);
            generateTestServerHelperLambda(s, func, *parser, ctx);
            generateTestServerSocketSetup(s, func, ctx);
            generateTestServerServiceCall(s, func, ServiceCallKind::Sync, ctx);

            ctx.m_out << "}" << endl << endl;

            // Should deliver exceptions for synchronous calls

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "void " << s.m_name << "Tester::shouldDeliver"
                    << exceptionTypeName << "In" << funcName << "()" << endl;

                ctx.m_out << "{" << endl;

                generateTestServerPrepareRequestParams(func, enumerations, ctx);
                generateTestServerPrepareRequestExceptionResponse(*parser, e, ctx);
                generateTestServerHelperLambda(s, func, *parser, ctx, e.m_name);
                generateTestServerSocketSetup(s, func, ctx);

                generateTestServerServiceCall(
                    s, func, ServiceCallKind::Sync, ctx, exceptionTypeName,
                    e.m_name);

                ctx.m_out << "}" << endl << endl;
            }

            // Should also properly deliver ThriftExceptions in synchronous
            // calls

            ctx.m_out << "void " << s.m_name
                << "Tester::shouldDeliverThriftExceptionIn" << funcName
                << "()" << endl;

            ctx.m_out << "{" << endl;

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

            ctx.m_out << "}" << endl << endl;

            // Should deliver request and response for successful asynchonous
            // calls

            ctx.m_out << "void " << s.m_name << "Tester::shouldExecute"
                << funcName << "Async()" << endl;

            ctx.m_out << "{" << endl;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestResponse(func, enumerations, ctx);
            generateTestServerHelperLambda(s, func, *parser, ctx);
            generateTestServerSocketSetup(s, func, ctx);
            generateTestServerServiceCall(s, func, ServiceCallKind::Async, ctx);

            ctx.m_out << "}" << endl << endl;

            // Should deliver exceptions for asynchronous calls

            for(const auto & e: func.m_throws)
            {
                auto exceptionTypeName = typeToStr(
                    e.m_type,
                    {},
                    MethodType::TypeName);

                ctx.m_out << "void " << s.m_name << "Tester::shouldDeliver"
                    << exceptionTypeName << "In" << funcName << "Async()" << endl;

                ctx.m_out << "{" << endl;

                generateTestServerPrepareRequestParams(func, enumerations, ctx);
                generateTestServerPrepareRequestExceptionResponse(*parser, e, ctx);
                generateTestServerHelperLambda(s, func, *parser, ctx, e.m_name);
                generateTestServerSocketSetup(s, func, ctx);

                generateTestServerServiceCall(
                    s, func, ServiceCallKind::Async, ctx, exceptionTypeName,
                    e.m_name);

                ctx.m_out << "}" << endl << endl;
            }

            // Should also properly deliver ThriftExceptions in synchronous
            // calls

            ctx.m_out << "void " << s.m_name
                << "Tester::shouldDeliverThriftExceptionIn" << funcName
                << "Async()" << endl;

            ctx.m_out << "{" << endl;

            generateTestServerPrepareRequestParams(func, enumerations, ctx);
            generateTestServerPrepareRequestExceptionResponse(
                *parser, exceptionField, ctx);

            generateTestServerHelperLambda(
                s, func, *parser, ctx, exceptionField.m_name);

            generateTestServerSocketSetup(s, func, ctx);

            generateTestServerServiceCall(
                s, func, ServiceCallKind::Async, ctx,
                QStringLiteral("ThriftException"), exceptionField.m_name);

            ctx.m_out << "}" << endl << endl;
        }

        writeNamespaceEnd(ctx.m_out);

        ctx.m_out << endl
            << "#include <Test" << s.m_name << ".moc>" << endl;
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

    ctx.m_out << blockSeparator << endl << endl;

    ctx.m_out << "QString generateRandomString(int len = 10);" << endl << endl
        << "qint8 generateRandomInt8();" << endl << endl
        << "qint16 generateRandomInt16();" << endl << endl
        << "qint32 generateRandomInt32();" << endl << endl
        << "qint64 generateRandomInt64();" << endl << endl
        << "quint8 generateRandomUint8();" << endl << endl
        << "quint16 generateRandomUint16();" << endl << endl
        << "quint32 generateRandomUint32();" << endl << endl
        << "quint64 generateRandomUint64();" << endl << endl
        << "double generateRandomDouble();" << endl << endl
        << "bool generateRandomBool();" << endl << endl;

    // Second section: generate random values of QEverCloud types

    ctx.m_out << blockSeparator << endl << endl;

    for(const auto & s: parser->structures())
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "();" << endl
            << endl;
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

    ctx.m_out << "namespace {" << endl << endl
        << blockSeparator << endl << endl;

    ctx.m_out << "static const QString randomStringAvailableCharacters = "
        << "QStringLiteral(" << endl
        << "    \""
        << "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        << "\");" << endl << endl;

    ctx.m_out << "template <typename T>" << endl
        << "T generateRandomIntType()" << endl
        << "{" << endl
        << "    T min = std::numeric_limits<T>::min() / 4;" << endl
        << "    T max = std::numeric_limits<T>::max() / 4;" << endl
        << "    return min + (rand() \% static_cast<T>(max - min + 1));"
        << endl
        << "}" << endl << endl;

    ctx.m_out << "} // namespace" << endl << endl;

    // Second section: generate random values of primitive types

    ctx.m_out << blockSeparator << endl << endl;

    ctx.m_out << "QString generateRandomString(int len)" << endl
        << "{" << endl
        << "    if (len <= 0) {" << endl
        << "        return {};" << endl
        << "    }" << endl << endl
        << "    QString res;" << endl
        << "    res.reserve(len);" << endl
        << "    for(int i = 0; i < len; ++i) {" << endl
        << "        int index = rand() % randomStringAvailableCharacters."
        << "length();" << endl
        << "        res.append(randomStringAvailableCharacters.at(index));"
        << "    }" << endl << endl
        << "    return res;" << endl
        << "}" << endl << endl;

    ctx.m_out << "qint8 generateRandomInt8()" << endl
        << "{" << endl
        << "    return generateRandomIntType<qint8>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "qint16 generateRandomInt16()" << endl
        << "{" << endl
        << "    return generateRandomIntType<qint16>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "qint32 generateRandomInt32()" << endl
        << "{" << endl
        << "    return generateRandomIntType<qint32>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "qint64 generateRandomInt64()" << endl
        << "{" << endl
        << "    return generateRandomIntType<qint64>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "quint8 generateRandomUint8()" << endl
        << "{" << endl
        << "    return generateRandomIntType<quint8>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "quint16 generateRandomUint16()" << endl
        << "{" << endl
        << "    return generateRandomIntType<quint16>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "quint32 generateRandomUint32()" << endl
        << "{" << endl
        << "    return generateRandomIntType<quint32>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "quint64 generateRandomUint64()" << endl
        << "{" << endl
        << "    return generateRandomIntType<quint64>();" << endl
        << "}" << endl << endl;

    ctx.m_out << "double generateRandomDouble()" << endl
        << "{" << endl
        << "    double minval = std::numeric_limits<double>::min();" << endl
        << "    double maxval = std::numeric_limits<double>::max();" << endl
        << "    double f = (double)rand() / RAND_MAX;" << endl
        << "    return minval + f * (maxval - minval);" << endl
        << "}" << endl << endl;

    ctx.m_out << "bool generateRandomBool()" << endl
        << "{" << endl
        << "    return generateRandomInt8() >= 0;" << endl
        << "}" << endl << endl;

    // Third section: generate random values of QEverCloud types

    ctx.m_out << blockSeparator << endl << endl;

    for(const auto & s: parser->structures())
    {
        ctx.m_out << s.m_name << " generateRandom" << s.m_name << "()" << endl
            << "{" << endl;

        ctx.m_out << "    " << s.m_name << " result;" << endl;

        for(const auto & f: s.m_fields) {
            generateGetRandomValueExpression(
                f,
                QStringLiteral("    result."),
                *parser,
                ctx.m_out);
        }

        ctx.m_out << "    return result;" << endl
            << "}" << endl << endl;
    }

    writeNamespaceEnd(ctx.m_out);
}

void Generator::generateLocalDataClassDeclaration(
    OutputFileContext & ctx)
{
    ctx.m_out << "/**" << endl
        << " * @brief The EverCloudLocalData class contains several" << endl
        << " * data elements which are not synchronized with Evernote service"
        << endl
        << " * but which are nevertheless useful in applications using" << endl
        << " * QEverCloud to implement feature rich full sync Evernote clients."
        << endl
        << " * Values of this class' types are contained within QEverCloud"
        << endl
        << " * types corresponding to actual Evernote API types" << endl
        << " */" << endl;

    ctx.m_out << "class QEVERCLOUD_EXPORT EverCloudLocalData"
        << ": public Printable" << endl
        << "{" << endl
        << "    Q_GADGET" << endl
        << "public:" << endl
        << "    EverCloudLocalData();" << endl
        << "    virtual ~EverCloudLocalData() noexcept override;" << endl << endl;

    ctx.m_out << "    virtual void print(QTextStream & strm) const override;"
        << endl << endl;

    ctx.m_out << "    bool operator==(const EverCloudLocalData & other) const;"
        << endl
        << "    bool operator!=(const EverCloudLocalData & other) const;"
        << endl;

    ctx.m_out << "    /**" << endl
        << "     * @brief id property can be used as a local unique identifier"
        << endl
        << "     * for any data item before it has been synchronized with"
        << endl
        << "     * Evernote and thus before it can be identified using its guid."
        << endl
        << "     *" << endl
        << "     * id property is generated automatically on EverCloudLocalData"
        << endl
        << "     * construction for convenience but can be overridden manually"
        << endl
        << "     */" << endl
        << "    QString id;" << endl << endl;

    ctx.m_out << "    /**" << endl
        << "     * @brief dirty property can be used to keep track which"
        << endl
        << "     * objects have been modified locally and thus need to be "
        << "synchronized" << endl
        << "     * with Evernote service" << endl
        << "     */" << endl
        << "    bool dirty = false;" << endl << endl;

    ctx.m_out << "    /**" << endl
        << "     * @brief local property can be used to keep track which"
        << endl
        << "     * data items are meant to be local only and thus never be "
        << "synchronized" << endl
        << "     * with Evernote service" << endl
        << "     */" << endl
        << "    bool local = false;" << endl << endl;

    ctx.m_out << "    /**" << endl
        << "     * @brief favorited property can be used to keep track which"
        << endl
        << "     * data items were favorited in the client. Unfortunately,"
        << endl
        << "     * Evernote has never provided a way to synchronize such"
        << endl
        << "     * property between different clients" << endl
        << "     */" << endl
        << "    bool favorited = false;" << endl << endl;

    ctx.m_out << "    /**" << endl
        << "     * @brief dict can be used for storage of any other auxiliary"
        << endl
        << "     * values associated with objects of QEverCloud types" << endl
        << "     */" << endl
        << "    QHash<QString, QVariant> dict;" << endl << endl;

    ctx.m_out << "    // Properties declaration for meta-object system" << endl
        << "    Q_PROPERTY(QString id MEMBER id USER true)" << endl
        << "    Q_PROPERTY(bool dirty MEMBER dirty)" << endl
        << "    Q_PROPERTY(bool local MEMBER local)" << endl
        << "    Q_PROPERTY(bool favorited MEMBER favorited)" << endl << endl
        << "    using Dict = QHash<QString, QVariant>;" << endl
        << "    Q_PROPERTY(Dict dict MEMBER dict)" << endl;

    ctx.m_out << "};" << endl;
}

void Generator::generateLocalDataClassDefinition(
    OutputFileContext & ctx)
{
    ctx.m_out << "EverCloudLocalData::EverCloudLocalData()" << endl
        << "{" << endl
        << "    id = QUuid::createUuid().toString();" << endl
        << "    // Remove curvy braces" << endl
        << "    id.remove(id.size() - 1, 1);" << endl
        << "    id.remove(0, 1);" << endl
        << "}" << endl << endl;

    ctx.m_out << "EverCloudLocalData::~EverCloudLocalData() noexcept" << endl
        << "{}" << endl << endl;

    ctx.m_out << "void EverCloudLocalData::print(QTextStream & strm) const"
        << endl
        << "{" << endl
        << "    strm << \"    localData.id = \" << id << \"\\n\"" << endl
        << "        << \"    localData.dirty = \" << (dirty ? \"true\" : \"false\")"
        << " << \"\\n\"" << endl
        << "        << \"    localData.local = \" << (local ? \"true\" : \"false\")"
        << " << \"\\n\"" << endl
        << "        << \"    localData.favorited = \" "
        << "<< (favorited ? \"true\" : \"false\")"
        << " << \"\\n\";" << endl << endl;

    ctx.m_out << "    if (!dict.isEmpty())" << endl
        << "    {" << endl
        << "        strm << \"    localData.dict:\" << \"\\n\";" << endl
        << "        QString valueStr;" << endl
        << "        for(const auto & it: toRange(dict)) {" << endl
        << "            strm << \"        [\" << it.key() << \"] = \";" << endl
        << "            valueStr.resize(0);" << endl
        << "            QDebug dbg(&valueStr);" << endl
        << "            dbg.noquote();" << endl
        << "            dbg.nospace();" << endl
        << "            dbg << it.value();" << endl
        << "            strm << valueStr << \"\\n\";" << endl
        << "        }" << endl
        << "    }" << endl;

    ctx.m_out << "}" << endl << endl;

    ctx.m_out << "bool EverCloudLocalData::operator==(" << endl
        << "    const EverCloudLocalData & other) const" << endl
        << "{" << endl
        << "    return id == other.id && dirty == other.dirty &&" << endl
        << "        local == other.local && favorited == other.favorited &&"
        << endl
        << "        dict == other.dict;" << endl
        << "}" << endl << endl
        << "bool EverCloudLocalData::operator!=(" << endl
        << "    const EverCloudLocalData & other) const" << endl
        << "{" << endl
        << "    return !operator==(other);" << endl
        << "}" << endl;
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
        << ": public I" << service.m_name << endl << "{" << endl;
    ctx.m_out << "    Q_OBJECT" << endl;
    ctx.m_out << "    Q_DISABLE_COPY(" << className << ")" << endl;
    ctx.m_out << "public:" << endl;

    ctx.m_out << "    explicit " << className << "(" << endl;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "            QString " << serviceName << "Url = {},"
            << endl;
    }
    else {
        ctx.m_out << "            I" << service.m_name << "Ptr service,"
            << endl;
    }

    ctx.m_out << "            IRequestContextPtr ctx = {}," << endl;

    if (serviceClassType == ServiceClassType::Durable) {
        ctx.m_out << "            IRetryPolicyPtr retryPolicy = newRetryPolicy(),"
            << endl;
    }

    ctx.m_out << "            QObject * parent = nullptr) :" << endl
        << "        I" << service.m_name << "(parent)," << endl;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "        m_url(std::move(" << serviceName << "Url)),"
            << endl;
    }
    else {
        ctx.m_out << "        m_service(std::move(service))," << endl
            << "        m_durableService(newDurableService("
            << "retryPolicy, ctx))," << endl;
    }

    ctx.m_out << "        m_ctx(std::move(ctx))" << endl
        << "    {" << endl
        << "        if (!m_ctx) {" << endl
        << "            m_ctx = newRequestContext();" << endl
        << "        }" << endl;

    if (serviceClassType == ServiceClassType::Durable) {
        ctx.m_out << endl
            << "        m_service->setParent(this);" << endl;
    }

    ctx.m_out << "    }" << endl
        << endl;

    if (serviceClassType == ServiceClassType::NonDurable)
    {
        ctx.m_out << "    explicit " << className
            << "(QObject * parent) :" << endl
            << "        I" << service.m_name << "(parent)" << endl
            << "    {" << endl
            << "        m_ctx = newRequestContext();" << endl
            << "    }" << endl
            << endl;

        ctx.m_out << "    virtual void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << endl
            << "    {" << endl
            << "        m_url = std::move(" << serviceName << "Url);" << endl
            << "    }"
            << endl << endl;

        ctx.m_out << "    virtual QString " << serviceName
            << "Url() const override" << endl
            << "    {" << endl
            << "        return m_url;" << endl
            << "    }"
            << endl << endl;
    }
    else
    {
        ctx.m_out << "    ~" << className << "()" << endl
            << "    {" << endl
            << "        // Don't interfere with std::shared_ptr's lifetime"
            << " tracking" << endl
            << "        m_service->setParent(nullptr);" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    virtual void set" << service.m_name
            << "Url(QString " << serviceName << "Url) override" << endl
            << "    {" << endl
            << "        m_service->set" << service.m_name << "Url("
            << serviceName << "Url);" << endl
            << "    }"
            << endl << endl;

        ctx.m_out << "    virtual QString " << serviceName
            << "Url() const override" << endl
            << "    {" << endl
            << "        return m_service->" << serviceName << "Url();" << endl
            << "    }"
            << endl << endl;
    }

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    virtual " << typeToStr(func.m_type, func.m_name) << " "
            << func.m_name << "(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << endl;
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

            ctx.m_out << "," << endl;
        }

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") override;" << endl << endl;

        ctx.m_out << "    virtual AsyncResult * " << func.m_name
            << "Async(" << endl;
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

            ctx.m_out << "," << endl;
        }

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") override;" << endl << endl;
    }

    ctx.m_out << "private:" << endl;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "    QString m_url;" << endl;
    }
    else {
        ctx.m_out << "    I" << service.m_name << "Ptr m_service;" << endl;
        ctx.m_out << "    IDurableServicePtr m_durableService;" << endl;
    }

    ctx.m_out << "    IRequestContextPtr m_ctx;" << endl;
    ctx.m_out << "};" << endl << endl;
}

void Generator::generateServiceClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & f: service.m_functions)
    {
        ctx.m_out << blockSeparator << endl << endl;

        QString prepareParamsName = service.m_name + capitalize(f.m_name) +
            QStringLiteral("PrepareParams");

        QString readReplyName = service.m_name + capitalize(f.m_name) +
            QStringLiteral("ReadReply");

        int lastId = f.m_params.last().m_id;

        bool isVoidResult =
            (std::dynamic_pointer_cast<Parser::VoidType>(f.m_type) != nullptr);

        ctx.m_out << "namespace {" << endl << endl;

        ctx.m_out << "QByteArray " << prepareParamsName << "(" << endl;
        for(const auto & param: f.m_params)
        {
            ctx.m_out << "    " << typeToStr(
                param.m_type,
                f.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            if (param.m_id != lastId) {
                ctx.m_out << "," << endl;
            }
        }

        ctx.m_out << ")" << endl;
        ctx.m_out << "{" << endl;

        auto logComponentName = camelCaseToSnakeCase(service.m_name);

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << prepareParamsName << "\");" << endl << endl;
        ctx.m_out << "    ThriftBinaryBufferWriter writer;" << endl;
        ctx.m_out << "    qint32 cseqid = 0;" << endl << endl;
        ctx.m_out << "    writer.writeMessageBegin(" << endl;
        ctx.m_out << "        QStringLiteral(\"" << f.m_name << "\")," << endl
            << "        ThriftMessageType::T_CALL," << endl
            << "        cseqid);" << endl << endl;
        ctx.m_out << "    writer.writeStructBegin(" << endl;
        ctx.m_out << "        QStringLiteral(\"" << service.m_name
            << "_" << f.m_name << "_pargs\"));" << endl;

        writeThriftWriteFields(
            ctx.m_out, f.m_params, f.m_name, QLatin1Literal(""));

        ctx.m_out << "    writer.writeFieldStop();" << endl;
        ctx.m_out << "    writer.writeStructEnd();" << endl;
        ctx.m_out << "    writer.writeMessageEnd();" << endl;
        ctx.m_out << "    return writer.buffer();" << endl;
        ctx.m_out << "}" << endl << endl;

        ctx.m_out << (isVoidResult
                      ? QStringLiteral("void")
                      : typeToStr(f.m_type, f.m_name))
            << " " << readReplyName << "(QByteArray reply)" << endl;
        ctx.m_out << "{" << endl;

        if (!isVoidResult) {
            ctx.m_out << "    bool resultIsSet = false;" << endl
                << "    " << typeToStr(f.m_type, f.m_name)
                << " result = " << typeToStr(f.m_type, f.m_name)
                << "();" << endl;
        }

        ctx.m_out << "    ThriftBinaryBufferReader reader(reply);" << endl
            << "    qint32 rseqid = 0;" << endl
            << "    QString fname;" << endl
            << "    ThriftMessageType mtype;" << endl
            << "    reader.readMessageBegin(fname, mtype, rseqid);" << endl
            << "    if (mtype == ThriftMessageType::T_EXCEPTION) {" << endl
            << "        ThriftException e = readThriftException(reader);" << endl
            << "        reader.readMessageEnd();" << endl
            << "        throw e;" << endl
            << "    }" << endl
            << "    if (mtype != ThriftMessageType::T_REPLY) {" << endl
            << "        reader.skip(ThriftFieldType::T_STRUCT);" << endl
            << "        reader.readMessageEnd();" << endl
            << "        throw ThriftException(ThriftException::Type::"
            << "INVALID_MESSAGE_TYPE);" << endl
            << "    }" << endl
            << "    if (fname.compare(QStringLiteral(\"" << f.m_name
            << "\")) != 0) {" << endl
            << "        reader.skip(ThriftFieldType::T_STRUCT);" << endl
            << "        reader.readMessageEnd();" << endl
            << "        throw ThriftException(ThriftException::Type::"
            << "WRONG_METHOD_NAME);" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    ThriftFieldType fieldType;" << endl
            << "    qint16 fieldId;" << endl
            << "    reader.readStructBegin(fname);" << endl
            << "    while(true)" << endl
            << "    {" << endl
            << "        reader.readFieldBegin(fname, fieldType, fieldId);" << endl
            << "        if (fieldType == ThriftFieldType::T_STOP) {" << endl
            << "            break;" << endl
            << "        }" << endl << endl;

        if (!isVoidResult)
        {
            Parser::Field result;
            result.m_id = 0;
            result.m_name = QStringLiteral("result");
            result.m_required = Parser::Field::RequiredFlag::Required;
            result.m_type = f.m_type;

            ctx.m_out << "        if (fieldId == 0)" << endl
                << "        {" << endl
                << "            if (fieldType == "
                << typeToStr(
                    f.m_type, f.m_name, MethodType::ThriftFieldType)
                << ") {" << endl
                << "                resultIsSet = true;" << endl;

            writeThriftReadField(
                ctx.m_out, result, f.m_name + QStringLiteral("."),
                QLatin1Literal(""));

            ctx.m_out << "            }" << endl
                << "            else {" << endl
                << "                reader.skip(fieldType);" << endl
                << "            }" << endl
                << "        }" << endl;
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

            ctx.m_out << "if (fieldId == "  << th.m_id << ")" << endl
                << "        {" << endl;

            QString exceptionType = typeToStr(
                th.m_type, f.m_name + QStringLiteral(", ") + th.m_name);

            ctx.m_out << "            if (fieldType == ThriftFieldType::"
                << "T_STRUCT) {" << endl
                << "                " << exceptionType << " e;" << endl
                << "                read" << exceptionType << "(reader, e);"
                << endl;

            if (exceptionType == QStringLiteral("EDAMSystemException")) {
                ctx.m_out << "                throwEDAMSystemException(e);"
                    << endl;
            }
            else {
                ctx.m_out << "                throw e;" << endl;
            }

            ctx.m_out << "            }" << endl
                << "            else {" << endl
                << "                reader.skip(fieldType);" << endl
                << "            }" << endl
                << "        }" << endl;
        }

        ctx.m_out << "        else" << endl
            << "        {" << endl
            << "            reader.skip(fieldType);" << endl
            << "        }" << endl << endl
            << "        reader.readFieldEnd();" << endl
            << "    }" << endl << endl
            << "    reader.readStructEnd();" << endl;

        ctx.m_out << "    reader.readMessageEnd();" << endl << endl;

        if (!isVoidResult) {
            ctx.m_out << "    if (!resultIsSet) {" << endl
                << "        throw ThriftException(" << endl
                << "            ThriftException::Type::"
                << "MISSING_RESULT," << endl
                << "            QStringLiteral(\""
                << f.m_name << ": missing result\"));" << endl
                << "    }" << endl << endl
                << "    return result;" << endl;
        }

        ctx.m_out << "}" << endl << endl;

        QString asyncReadFunctionName =
            readReplyName + QStringLiteral("Async");
        ctx.m_out << "QVariant " << asyncReadFunctionName
            << "(QByteArray reply)" << endl;
        ctx.m_out << "{" << endl;
        if (isVoidResult) {
            ctx.m_out << "    " << readReplyName << "(reply);" << endl
                << "    return QVariant();" << endl;
        }
        else {
            ctx.m_out << "    return QVariant::fromValue(" << readReplyName
                << "(reply));" << endl;
        }
        ctx.m_out << "}" << endl << endl;

        ctx.m_out << "} // namespace" << endl << endl;

        ctx.m_out << typeToStr(f.m_type, f.m_name) << " "
            << service.m_name << "::" << f.m_name << "(" << endl;
        for(const auto & param: f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type, f.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            ctx.m_out << "," << endl;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << endl
            << "{" << endl;

        ctx.m_out << "    if (!ctx) {" << endl
            << "        ctx.reset(m_ctx->clone());" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << service.m_name << "::" << f.m_name << ": request id = \""
            << endl << "        << ctx->requestId());" << endl;

        auto loggableParams = loggableFields(f.m_params);
        if (!loggableParams.isEmpty())
        {
            ctx.m_out << "    QEC_TRACE(\"" << logComponentName
                << "\", \"Parameters:\\n\"" << endl;
            auto lastLoggableParamId = loggableParams.last().m_id;
            for(const auto & param: loggableParams)
            {
                ctx.m_out << "        << \"    " << param.m_name << " = \" << "
                    << param.m_name;
                if (param.m_id == lastLoggableParamId) {
                    ctx.m_out << ");" << endl;
                }
                else {
                    ctx.m_out << " << \"\\n\"" << endl;
                }
            }
        }

        ctx.m_out << endl;

        ctx.m_out << "    QByteArray params = " << prepareParamsName << "("
            << endl;
        for(const auto & param : f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                ctx.m_out << "        ctx->authenticationToken()";
            }
            else {
                ctx.m_out << "        " << param.m_name;
            }

            if (param.m_id != lastId) {
                ctx.m_out << "," << endl;
            }
        }
        ctx.m_out << ");" << endl << endl;

        ctx.m_out << "    QByteArray reply = askEvernote(" << endl
            << "        m_url," << endl
            << "        params," << endl
            << "        ctx->requestTimeout());" << endl << endl;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << "received reply for request with id = \"" << endl
            << "        << ctx->requestId());" << endl;

        if (isVoidResult) {
            ctx.m_out << "    " << readReplyName << "(reply);" << endl;
        }
        else {
            ctx.m_out << "    return " << readReplyName << "(reply);"
                << endl;
        }

        ctx.m_out << "}" << endl << endl;

        ctx.m_out << "AsyncResult * " << service.m_name << "::" << f.m_name
            << "Async(" << endl;
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

            ctx.m_out << "," << endl;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << endl
            << "{" << endl;

        ctx.m_out << "    QEC_DEBUG(\"" << logComponentName << "\", \""
            << service.m_name << "::" << f.m_name << "Async\");" << endl;
        if (!loggableParams.isEmpty())
        {
            ctx.m_out << "    QEC_TRACE(\"" << logComponentName
                << "\", \"Parameters:\\n\"" << endl;
            auto lastLoggableParamId = loggableParams.last().m_id;
            for(const auto & param: loggableParams)
            {
                ctx.m_out << "        << \"    " << param.m_name << " = \" << "
                    << param.m_name;
                if (param.m_id == lastLoggableParamId) {
                    ctx.m_out << ");" << endl;
                }
                else {
                    ctx.m_out << " << \"\\n\"" << endl;
                }
            }
        }

        ctx.m_out << endl;
        ctx.m_out << "    if (!ctx) {" << endl
            << "        ctx.reset(m_ctx->clone());" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    QByteArray params = " << prepareParamsName << "("
            << endl;
        for(const auto & param: f.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                ctx.m_out << "        ctx->authenticationToken()";
            }
            else {
                ctx.m_out << "        " << param.m_name;
            }

            if (param.m_id != lastId) {
                ctx.m_out << "," << endl;
            }
        }
        ctx.m_out << ");" << endl << endl
            << "    return new AsyncResult(" << endl
            << "        m_url," << endl
            << "        params," << endl
            << "        ctx," << endl
            << "        " << asyncReadFunctionName << ");" << endl;

        ctx.m_out << "}" << endl << endl;
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
            << "Durable" << service.m_name << "::" << func.m_name << "(" << endl;
        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "    " << typeToStr(
                param.m_type, func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::FuncParamType);
            ctx.m_out << " " << param.m_name;
            ctx.m_out << "," << endl;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << endl
            << "{" << endl;

        ctx.m_out << "    if (!ctx) {" << endl
            << "        ctx.reset(m_ctx->clone());" << endl
            << "    }" << endl << endl;

        bool isVoidResult =
            (std::dynamic_pointer_cast<Parser::VoidType>(func.m_type) != nullptr);

        ctx.m_out << "    auto call = "
            << "IDurableService::SyncServiceCall(" << endl
            << "        [&] (IRequestContextPtr ctx)" << endl
            << "        {" << endl;

        ctx.m_out << "            ";
        if (!isVoidResult) {
            ctx.m_out << "auto res = ";
        }

        ctx.m_out << "m_service->" << func.m_name << "(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << endl;
        }

        for(const auto & param: qAsConst(func.m_params))
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << endl;
        }

        ctx.m_out << "                ctx);" << endl;

        ctx.m_out << "            return IDurableService::SyncResult(QVariant";
        if (!isVoidResult) {
            ctx.m_out << "::fromValue(res)";
        }
        else {
            ctx.m_out << "()";
        }
        ctx.m_out << ", {});" << endl
            << "        });" << endl << endl;

        bool requestDescriptionIsEmpty = false;
        auto loggableParams = loggableFields(func.m_params);

        if (loggableParams.isEmpty())
        {
            requestDescriptionIsEmpty = true;
        }
        else
        {
            ctx.m_out << "    QString requestDescription;" << endl
                      << "    QTextStream strm(&requestDescription);" << endl;

            ctx.m_out << "    if (logger()->shouldLog(LogLevel::Trace, "
                << "\"durable_service\")) {" << endl;

            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        strm << \"" << param.m_name << " = \" << "
                          << param.m_name << " << \"\\n\";" << endl;
            }
            ctx.m_out << "    }" << endl << endl;
        }

        ctx.m_out << "    IDurableService::SyncRequest request(" << endl
            << "        \"" << func.m_name << "\"," << endl;

        if (!requestDescriptionIsEmpty) {
            ctx.m_out << "        requestDescription," << endl;
        }
        else {
            ctx.m_out << "        {}," << endl;
        }

        ctx.m_out << "        std::move(call));" << endl << endl;

        ctx.m_out << "    auto result = m_durableService->executeSyncRequest("
            << endl
            << "        std::move(request), ctx);" << endl << endl;

        ctx.m_out << "    if (result.second) {" << endl
            << "        result.second->throwException();" << endl
            << "    }" << endl << endl;

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
        ctx.m_out << ";" << endl
            << "}" << endl << endl;

        // Asynchronous version

        ctx.m_out << "AsyncResult * Durable" << service.m_name << "::"
            << func.m_name << "Async(" << endl;
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

            ctx.m_out << "," << endl;
        }

        ctx.m_out << "    IRequestContextPtr ctx";
        ctx.m_out << ")" << endl
            << "{" << endl;

        ctx.m_out << "    if (!ctx) {" << endl
            << "        ctx.reset(m_ctx->clone());" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    auto call = "
            << "IDurableService::AsyncServiceCall(" << endl
            << "        [=, service=m_service] (IRequestContextPtr ctx)" << endl
            << "        {" << endl
            << "            return service->" << func.m_name << "Async("
            << endl;

        for(const auto & param : func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                continue;
            }

            ctx.m_out << "                " << param.m_name << "," << endl;
        }
        ctx.m_out << "                ctx);" << endl
            << "        });" << endl << endl;

        if (!requestDescriptionIsEmpty)
        {
            ctx.m_out << "    QString requestDescription;" << endl
                      << "    QTextStream strm(&requestDescription);" << endl;

            ctx.m_out << "    if (logger()->shouldLog(LogLevel::Trace, "
                << "\"durable_service\")) {" << endl;

            for(const auto & param: qAsConst(loggableParams))
            {
                ctx.m_out << "        strm << \"" << param.m_name << " = \" << "
                          << param.m_name << " << \"\\n\";" << endl;
            }
            ctx.m_out << "    }" << endl << endl;
        }

        ctx.m_out << "    IDurableService::AsyncRequest request(" << endl
            << "        \"" << func.m_name << "\"," << endl;

        if (!requestDescriptionIsEmpty) {
            ctx.m_out  << "        requestDescription," << endl;
        }
        else {
            ctx.m_out << "        {}," << endl;
        }

        ctx.m_out  << "        std::move(call));" << endl << endl;

        ctx.m_out << "    return m_durableService->executeAsyncRequest("
            << endl
            << "        std::move(request), ctx);" << endl << endl;

        ctx.m_out << "}" << endl << endl;
    }
}

void Generator::generateServerClassDeclaration(
    const Parser::Service & service, OutputFileContext & ctx)
{
    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    ctx.m_out << blockSeparator << endl << endl;

    ctx.m_out << "/**" << endl
        << " * @brief The " << service.m_name << "Server class represents"
        << endl
        << " * customizable server for " << service.m_name << " requests."
        << endl
        << " * It is primarily used for testing of QEverCloud" << endl
        << " */" << endl;

    ctx.m_out << "class QEVERCLOUD_EXPORT " << service.m_name
        << "Server: public QObject" << endl << "{" << endl;
    ctx.m_out << "    Q_OBJECT" << endl;
    ctx.m_out << "    Q_DISABLE_COPY(" << service.m_name << "Server)" << endl;

    ctx.m_out << "public:" << endl
        << "    explicit " << service.m_name
        << "Server(QObject * parent = nullptr);" << endl << endl;

    ctx.m_out << "Q_SIGNALS:" << endl;
    ctx.m_out << "    // Signals notifying listeners about incoming requests"
        << endl;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void " << func.m_name << "Request(";
        if (!func.m_params.isEmpty()) {
            ctx.m_out << endl;
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
            ctx.m_out << paramType << " " << param.m_name << "," << endl;
        }

        if (!func.m_params.isEmpty()) {
            ctx.m_out << "        ";
        }
        ctx.m_out << "IRequestContextPtr ctx);" << endl << endl;
    }

    ctx.m_out << "    // Signals used to send encoded response data" << endl;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void " << func.m_name << "RequestReady(" << endl
            << "        QByteArray data);" << endl << endl;
    }

    ctx.m_out << "public Q_SLOTS:" << endl;
    ctx.m_out << "    // Slot used to deliver requests to the server" << endl
        << "    void onRequest(QByteArray data);" << endl << endl;

    ctx.m_out << "    // Slots for replies to requests" << endl;
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "    void on" << capitalize(func.m_name) << "RequestReady("
            << endl;
        auto responseType = typeToStr(func.m_type, func.m_name);
        if (responseType != QStringLiteral("void")) {
            ctx.m_out << "        " << responseType << " value," << endl;
        }
        ctx.m_out << "        EverCloudExceptionDataPtr "
            << "exceptionData);" << endl << endl;
    }

    ctx.m_out << "};" << endl << endl;
}

void Generator::generateServerClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    if (!service.m_extends.isEmpty()) {
        throw std::runtime_error("extending services is not supported");
    }

    ctx.m_out << blockSeparator << endl << endl;

    ctx.m_out << service.m_name << "Server::" << service.m_name
        << "Server(QObject * parent) :" << endl
        << "    QObject(parent)" << endl
        << "{}" << endl << endl;

    ctx.m_out << "void " << service.m_name << "Server::onRequest(QByteArray data)"
        << endl
        << "{" << endl
        << "    ThriftBinaryBufferReader reader(data);" << endl
        << "    qint32 rseqid = 0;" << endl
        << "    QString fname;" << endl
        << "    ThriftMessageType mtype;" << endl
        << "    reader.readMessageBegin(fname, mtype, rseqid);" << endl << endl;

    ctx.m_out << "    if (mtype != ThriftMessageType::T_CALL) {" << endl
        << "        reader.skip(ThriftFieldType::T_STRUCT);" << endl
        << "        reader.readMessageEnd();" << endl
        << "        throw ThriftException("
        << "ThriftException::Type::INVALID_MESSAGE_TYPE);" << endl
        << "    }" << endl << endl;

    bool firstFunc = true;
    for (const auto & func: qAsConst(service.m_functions))
    {
        ctx.m_out << "    ";

        if (!firstFunc) {
            ctx.m_out << "else ";
        }

        ctx.m_out << "if (fname == QStringLiteral(\"" << func.m_name
            << "\"))" << endl
            << "    {" << endl;

        quint32 paramCount = 0;
        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "        " << typeToStr(
                param.m_type,
                func.m_name + QStringLiteral(", ") + param.m_name,
                MethodType::TypeName);
            ctx.m_out << " " << param.m_name << ";" << endl;

            ++paramCount;
        }

        ctx.m_out << "        IRequestContextPtr ctx;" << endl << endl;

        ctx.m_out << "        parse" << capitalize(service.m_name)
            << capitalize(func.m_name) << "Params(" << endl;

        ctx.m_out << "            reader," << endl;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "            " << param.m_name << "," << endl;
        }

        ctx.m_out << "            ctx);" << endl << endl;

        ctx.m_out << "        Q_EMIT " << func.m_name << "Request(" << endl;

        for(const auto & param: func.m_params)
        {
            if (param.m_name == QStringLiteral("authenticationToken")) {
                // Auth token is a part of IRequestContext interface
                continue;
            }

            ctx.m_out << "            " << param.m_name;
            ctx.m_out << "," << endl;
        }

        ctx.m_out << "            ctx);" << endl;
        ctx.m_out << "    }" << endl;

        firstFunc = false;
    }

    ctx.m_out << "}" << endl << endl;

    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << "void " << service.m_name << "Server::on"
            << capitalize(func.m_name) << "RequestReady(" << endl;

        auto responseTypeName = typeToStr(func.m_type, func.m_name);
        if (responseTypeName != QStringLiteral("void")) {
            ctx.m_out << "    " << responseTypeName << " value," << endl;
        }
        ctx.m_out << "    EverCloudExceptionDataPtr exceptionData)"
            << endl;

        ctx.m_out << "{" << endl;

        ctx.m_out << "    ThriftBinaryBufferWriter writer;" << endl
            << "    qint32 cseqid = 0;" << endl << endl;

        ctx.m_out << "    if (exceptionData)" << endl
            << "    {" << endl
            << "        try" << endl
            << "        {" << endl
            << "            exceptionData->throwException();" << endl
            << "        }" << endl
            << "        catch(const ThriftException & exception)" << endl
            << "        {" << endl
            << "            writer.writeMessageBegin(" << endl
            << "                QStringLiteral(\"" << func.m_name << "\"),"
            << endl
            << "                ThriftMessageType::T_EXCEPTION," << endl
            << "                cseqid);" << endl
            << "            writeThriftException(writer, exception);" << endl
            << "            writer.writeMessageEnd();" << endl << endl
            << "            Q_EMIT " << func.m_name << "RequestReady(" << endl
            << "                writer.buffer());" << endl
            << "            return;" << endl
            << "        }" << endl
            << "        catch(...)" << endl
            << "        {" << endl
            << "            // Will be handled below" << endl
            << "        }" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    writer.writeMessageBegin(" << endl
            << "        QStringLiteral(\"" << func.m_name << "\")," << endl
            << "        ThriftMessageType::T_REPLY," << endl
            << "        cseqid);" << endl << endl;

        ctx.m_out << "    writer.writeStructBegin(" << endl
            << "        QStringLiteral(\"" << func.m_name << "\"));" << endl << endl;

        if (!func.m_throws.isEmpty())
        {
            ctx.m_out << "    if (exceptionData)" << endl
                << "    {" << endl;

            ctx.m_out << "        try" << endl
                << "        {" << endl
                << "            exceptionData->throwException();" << endl
                << "        }" << endl;

            for(const auto & th: func.m_throws)
            {
                QString exceptionType = typeToStr(th.m_type, {});

                ctx.m_out << "        catch(const " << exceptionType << " & e)"
                    << endl
                    << "        {" << endl
                    << "            writer.writeFieldBegin(" << endl
                    << "                QStringLiteral(\"" << exceptionType
                    << "\")," << endl
                    << "                ThriftFieldType::T_STRUCT," << endl
                    << "                " << th.m_id << ");" << endl << endl;

                ctx.m_out << "            write" << exceptionType
                    << "(writer, e);" << endl
                    << "            writer.writeFieldEnd();" << endl << endl
                    << "            // Finalize message and return immediately"
                    << endl
                    << "            writer.writeStructEnd();" << endl
                    << "            writer.writeMessageEnd();" << endl << endl
                    << "            Q_EMIT " << func.m_name << "RequestReady("
                    << endl
                    << "                writer.buffer());" << endl
                    << "            return;" << endl
                    << "        }" << endl;
            }

            ctx.m_out << "        catch(const std::exception & e)" << endl
                << "        {" << endl
                << "            // TODO: more proper error handling" << endl
                << "            QEC_ERROR(\"server\", \"Unknown exception: \""
                << " << e.what());" << endl
                << "        }" << endl;

            ctx.m_out << "        catch(...)" << endl
                << "        {" << endl
                << "            // TODO: more proper error handling" << endl
                << "            QEC_ERROR(\"server\", \"Unknown exception\");"
                << endl
                << "        }" << endl;

            ctx.m_out << "    }" << endl << endl;
        }

        ctx.m_out << "    writer.writeFieldBegin(" << endl
            << "        QStringLiteral(\"" << func.m_name << "\")," << endl
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

        ctx.m_out << "," << endl << "        0);" << endl;

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
                ctx.m_out << ", value.size());" << endl;

                ctx.m_out << "    for(const auto & v: qAsConst(value)) {"
                    << endl << "        ";

                ctx.m_out << typeToStr(
                    listType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "v);" << endl
                    << "    }" << endl
                    << "    writer.writeListEnd();" << endl;
            }
            else if (setType)
            {
                ctx.m_out << typeToStr(
                    listType->m_valueType,
                    func.m_name,
                    MethodType::ThriftFieldType);
                ctx.m_out << ", value.size());" << endl;

                ctx.m_out << "    for(const auto & v: qAsConst(value)) {"
                    << endl << "        ";

                ctx.m_out << typeToStr(
                    setType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "v);" << endl
                    << "    }" << endl
                    << "    writer.writeSetEnd();" << endl;
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
                ctx.m_out << ", value);" << endl;

                ctx.m_out << "    for(const auto & it: toRange(qAsConst(value))) {"
                    << endl << "        ";

                ctx.m_out << typeToStr(
                    mapType->m_keyType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "it.key());" << endl;

                ctx.m_out << typeToStr(
                    mapType->m_valueType,
                    func.m_name,
                    MethodType::WriteMethod);
                ctx.m_out << "it.value());" << endl;

                ctx.m_out << "    }" << endl
                    << "    writer.writeMapEnd();" << endl;
            }
            else
            {
                ctx.m_out << "value);" << endl;
            }
        }

        ctx.m_out << "    writer.writeFieldEnd();" << endl << endl;

        ctx.m_out << "    writer.writeFieldBegin(QString(), "
            << "ThriftFieldType::T_STOP, 0);" << endl
            << "    writer.writeFieldEnd();" << endl << endl;

        ctx.m_out << "    writer.writeStructEnd();" << endl
            << "    writer.writeMessageEnd();" << endl << endl;

        ctx.m_out << "    Q_EMIT " << func.m_name << "RequestReady(" << endl
            << "        writer.buffer());" << endl;

        ctx.m_out << "}" << endl << endl;
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

        ctx.m_out << blockSeparator << endl << endl;

        ctx.m_out << "void parse" << capitalize(service.m_name)
            << capitalize(func.m_name) << "Params(" << endl
            << "    ThriftBinaryBufferReader & reader," << endl;

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
                << "," << endl;
        }

        ctx.m_out << "    IRequestContextPtr & ctx)" << endl
            << "{" << endl;

        ctx.m_out << "    ThriftFieldType fieldType;" << endl
            << "    qint16 fieldId;" << endl;

        if (hasAuthenticationToken) {
            ctx.m_out << "    QString authenticationToken;" << endl;
        }

        ctx.m_out << endl;
        ctx.m_out << "    QString fname =" << endl
            << "        QStringLiteral(\""
            << service.m_name << "_" << func.m_name << "_pargs\");" << endl;


        ctx.m_out << endl;
        ctx.m_out << "    reader.readStructBegin(fname);" << endl
            << "    while(true)" << endl
            << "    {" << endl
            << "        reader.readFieldBegin(fname, fieldType, fieldId);"
            << endl
            << "        if (fieldType == ThriftFieldType::T_STOP) {" << endl
            << "            break;" << endl
            << "        }" << endl << endl;

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
                << endl
                << "        {" << endl
                << "            if (fieldType == "
                << typeToStr(
                    param.m_type, param.m_name, MethodType::ThriftFieldType)
                << ") {" << endl;

            writeThriftReadField(
                ctx.m_out, param, param.m_name + QStringLiteral("."),
                QLatin1String(""));

            ctx.m_out << "            }" << endl
                << "            else {" << endl
                << "                reader.skip(fieldType);" << endl
                << "            }" << endl
                << "        }" << endl;
        }

        ctx.m_out << "        else" << endl
            << "        {" << endl
            << "            reader.skip(fieldType);" << endl
            << "        }" << endl << endl
            << "        reader.readFieldEnd();" << endl;

        ctx.m_out << "    }" << endl << endl
            << "    reader.readStructEnd();" << endl
            << "    reader.readMessageEnd();" << endl << endl;

        ctx.m_out << "    ctx = newRequestContext(";
        if (hasAuthenticationToken) {
            ctx.m_out << "authenticationToken";
        }
        ctx.m_out << ");" << endl << "}" << endl << endl;
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
        s.replace(QStringLiteral("\""), QLatin1Literal(""));
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

