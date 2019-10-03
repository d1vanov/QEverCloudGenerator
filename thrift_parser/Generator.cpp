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

void Generator::writeEnumeration(
    QTextStream & out, const Parser::Enumeration & e) const
{
    if (!e.m_docComment.isEmpty()) {
        out << e.m_docComment << endl;
    }

    out << "enum class " << e.m_name << endl << "{" << endl;

    size_t i = 0;
    size_t numValues = e.m_values.size();
    for(const auto & v: e.m_values)
    {
        out << "    " << v.first;

        if (!v.second.isEmpty()) {
            out << " = " << v.second;
        }

        if (i < (numValues - 1)) {
            out << ",";
        }

        out << endl;
        ++i;
    }

    out << "};" << endl << endl;

    out << "inline uint qHash(" << e.m_name << " value)"
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

void Generator::writeHeaderHeader(
    QTextStream & out, const QString & fileName,
    const QStringList & additionalIncludes,
    const HeaderKind headerKind)
{
    out << disclaimer << endl;

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split(QChar::fromLatin1('.'))[0].toUpper());
    out << "#ifndef " << guard << endl;
    out << "#define " << guard << endl;
    out << endl;

    if (headerKind == HeaderKind::Public) {
        out << "#include \"../Export.h\"" << endl;
        out << endl;
    }

    for(const auto & include: qAsConst(additionalIncludes))
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            out << "#include " << include << endl;
        }
        else {
            out << "#include \"" << include << "\"" << endl;
        }
    }

    if (!additionalIncludes.isEmpty()) {
        out << endl;
    }

    out << "namespace qevercloud {";
    out << endl;
    out << endl;
}

void Generator::writeHeaderBody(
    QTextStream & out, const QString & headerFileName,
    const QStringList & additionalIncludes)
{
    out << disclaimer << endl;
    out << "#include <generated/" << headerFileName << ">" << endl;
    out << "#include \"../Impl.h\"" << endl;

    for(const auto & include: additionalIncludes)
    {
        if (include.startsWith(QChar::fromLatin1('<'))) {
            out << "#include " << include << endl;
        }
        else {
            out << "#include \"" << include << "\"" << endl;
        }
    }

    out << endl;
    out << "namespace qevercloud {";
    out << endl;
    out << endl;
    out << blockSeparator;
    out << endl;
    out << endl;
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

    out << "} // namespace qevercloud" << endl;

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

void Generator::writeBodyFooter(QTextStream & out)
{
    out << "} // namespace qevercloud" << endl;
}

QString Generator::typeToStr(
    QSharedPointer<Parser::Type> type, const QString & identifier,
    const MethodType methodType)
{
    QSharedPointer<Parser::BaseType> baseType =
        type.dynamicCast<Parser::BaseType>();

    QSharedPointer<Parser::VoidType> voidType =
        type.dynamicCast<Parser::VoidType>();

    QSharedPointer<Parser::IdentifierType> identifierType =
        type.dynamicCast<Parser::IdentifierType>();

    QSharedPointer<Parser::MapType> mapType =
        type.dynamicCast<Parser::MapType>();

    QSharedPointer<Parser::SetType> setType =
        type.dynamicCast<Parser::SetType>();

    QSharedPointer<Parser::ListType> listType =
        type.dynamicCast<Parser::ListType>();

    QString result;

    QString typeName;
    if (methodType == MethodType::FuncParamType) {
        typeName = typeToStr(type, identifier, MethodType::TypeName);
    }

    if (!baseType.isNull())
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
                result = QStringLiteral("w.writeBool(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readBool(");
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
                result = QStringLiteral("w.writeString(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readString(");
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
                result = QStringLiteral("w.writeDouble(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readDouble(");
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
                result = QStringLiteral("w.writeBinary(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readBinary(");
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
                result = QStringLiteral("w.writeByte(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readByte(");
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
                result = QStringLiteral("w.writeI16(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readI16(");
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
                result = QStringLiteral("w.writeI32(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readI32(");
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
                result = QStringLiteral("w.writeI64(");
                break;
            case MethodType::ReadMethod:
                result = QStringLiteral("r.readI64(");
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
        else if(methodType == MethodType::ReadTypeName)
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

                QSharedPointer<Parser::BaseType> type2(new Parser::BaseType);
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
                            strm << "write" << nameOfType << "(w, ";
                        }
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "read" << nameOfType << "(r, ";
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
                        result = QStringLiteral("w.writeI32(static_cast<qint32>(");
                        break;
                    case MethodType::ReadMethod:
                        {
                            QTextStream strm(&result);
                            strm << "readEnum" << nameOfType << "(r, ";
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
            result = QStringLiteral("w.writeMapBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("r.readMapBegin(");
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
            result = QStringLiteral("w.writeSetBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("r.readSetBegin(");
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
            result = QStringLiteral("w.writeListBegin(");
            break;
        case MethodType::ReadMethod:
            result = QStringLiteral("r.readListBegin(");
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
    QSharedPointer<Parser::ConstValue> value,
    QSharedPointer<Parser::Type> type, const QString & identifier,
    const QString & offset)
{
    if (value.isNull()) {
        return QString();
    }

    QSharedPointer<Parser::MapType> mapType =
        type.dynamicCast<Parser::MapType>();

    QSharedPointer<Parser::SetType> setType =
        type.dynamicCast<Parser::SetType>();

    QSharedPointer<Parser::ListType> listType =
        type.dynamicCast<Parser::ListType>();

    QSharedPointer<Parser::StringValue> stringValue =
        value.dynamicCast<Parser::StringValue>();

    QSharedPointer<Parser::LiteralValue> literalValue =
        value.dynamicCast<Parser::LiteralValue>();

    QSharedPointer<Parser::ListValue> listValue =
        value.dynamicCast<Parser::ListValue>();

    QSharedPointer<Parser::MapValue> mapValue =
        value.dynamicCast<Parser::MapValue>();

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
                "List initializer for a unsupported type for (%1)")
                .arg(identifier).toStdString());
        }

        result = typeToStr(type, identifier) + QStringLiteral("()");
        QString nextOffset = offset + QStringLiteral("    ");
        QTextStream strm(&result, QIODevice::Append);
        strm << endl;
        for(const auto & v: qAsConst(listValue->m_values)) {
            strm << offset << "<< "
                << valueToStr(v, QSharedPointer<Parser::Type>(nullptr),
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

    writeHeaderHeader(ctx.m_out, fileName);

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

    writeHeaderBody(
        ctx.m_out, QStringLiteral("Constants.h"), additionalIncludes);

    const auto & constants = parser->constants();
    for(const auto & c: constants)
    {
        if (c.m_fileName != fileName) {
            ctx.m_out << "// " << c.m_fileName << endl << endl;
        }

        if (c.m_value.isNull()) {
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
    writeBodyFooter(ctx.m_out);
}

QString Generator::fieldToStr(const Parser::Field & field)
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

QString Generator::getIdentifier(const QSharedPointer<Parser::Type> & type)
{
    auto it = type.dynamicCast<Parser::IdentifierType>();
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

        out << ident << "    w.writeFieldBegin(" << endl
            << ident << "        QStringLiteral(\""
            << field.m_name << "\")," << endl
            << ident << "        " << typeToStr(
                field.m_type, identPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << "," << endl
            << ident << "        " << field.m_id << ");" << endl;

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
            QSharedPointer<Parser::Type> valueType =
                field.m_type.dynamicCast<Parser::ListType>()->m_valueType;

            out << ident << "    w.writeListBegin("
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
            out << ident << "    w.writeListEnd();" << endl;
        }
        else if (writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            QSharedPointer<Parser::Type> valueType =
                field.m_type.dynamicCast<Parser::SetType>()->m_valueType;

            out << ident << "    w.writeSetBegin("
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
            out << ident << "    w.writeSetEnd();" << endl;
        }
        else if (writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            QSharedPointer<Parser::Type> keyType =
                field.m_type.dynamicCast<Parser::MapType>()->m_keyType;

            QSharedPointer<Parser::Type> valueType =
                field.m_type.dynamicCast<Parser::MapType>()->m_valueType;

            out << ident << "    w.writeMapBegin("
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
            out << ident << "    w.writeMapEnd();" << endl;
        }
        else
        {
            out << ident << "    " << writeMethod << fieldMoniker
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << ");" << endl;
        }

        out << ident << "    w.writeFieldEnd();" << endl;
        if (isOptional) {
            out << "    }" << endl;
        }
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
        QSharedPointer<Parser::Type> valueType =
            field.m_type.dynamicCast<Parser::ListType>()->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType,  identPrefix + field.m_name,
            MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readListBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if(elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect list type ("
            << identPrefix + field.m_name << ")\"));" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {"
            << endl;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.append(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "r.readListEnd();" << endl;
    }
    else if (readMethod.contains(QStringLiteral("readSetBegin")))
    {
        QSharedPointer<Parser::Type> valueType =
            field.m_type.dynamicCast<Parser::SetType>()->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readSetBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if (elemType != " << valueThriftType
            << ") throw ThriftException(ThriftException::Type::"
            << "INVALID_DATA, QStringLiteral(\"Incorrect set type ("
            << identPrefix + field.m_name << ")\"));" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    "
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << " elem;" << endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.insert(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "r.readSetEnd();" << endl;
    }
    else if (readMethod.contains(QStringLiteral("readMapBegin")))
    {
        QSharedPointer<Parser::Type> keyType =
            field.m_type.dynamicCast<Parser::MapType>()->m_keyType;

        QString keyReadMethod = typeToStr(
            keyType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString keyThriftType = typeToStr(
            keyType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        QSharedPointer<Parser::Type> valueType =
            field.m_type.dynamicCast<Parser::MapType>()->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type keyType;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readMapBegin(keyType, elemType, size);" << endl;
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
        out << indent << "r.readMapEnd();" << endl;
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
        << QStringLiteral("<QTextStream>") << QStringLiteral("<QDebug>");

    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx.m_out, fileName, additionalIncludes);

    const auto & enumerations = parser->enumerations();
    int enumerationsCount = enumerations.size();
    int i = 0;
    for(const auto & e: enumerations)
    {
        writeEnumeration(ctx.m_out, e);
        ctx.m_out << blockSeparator << endl << endl;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QTextStream");
        ctx.m_out << blockSeparator << endl << endl;

        writeEnumerationPrintDeclaration(ctx.m_out, e, "QDebug");
        if (i != (enumerationsCount - 1)) {
            ctx.m_out << blockSeparator << endl << endl;
        }

        ++i;
    }

    writeHeaderFooter(ctx.m_out, fileName);
}

void Generator::generateErrorsCpp(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("EDAMErrorCode.cpp");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Implementation);

    writeHeaderBody(ctx.m_out, QStringLiteral("EDAMErrorCode.h"));

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

    writeBodyFooter(ctx.m_out);
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
        ctx.m_out, fileName, additionalIncludes, HeaderKind::Private);

    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    QList<const QList<Parser::Structure>*> lists;
    lists.reserve(2);
    lists << &parser->structures();
    lists << &parser->exceptions();

    for(const auto & pList: qAsConst(lists))
    {
        for(const auto & s: *pList) {
            ctx.m_out << "void write" << s.m_name
                << "(ThriftBinaryBufferWriter & w, const "
                << s.m_name << " & s);" << endl;
            ctx.m_out << "void read" << s.m_name
                << "(ThriftBinaryBufferReader & r, "
                << s.m_name << " & s);" << endl;
        }
    }
    ctx.m_out << endl;

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations) {
        ctx.m_out << "void readEnum" << e.m_name
            << "(ThriftBinaryBufferReader & r, "
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
        << QStringLiteral("EDAMErrorCode.h") << QStringLiteral("../Optional.h")
        << QStringLiteral("<QSharedPointer>") << QStringLiteral("<QMetaType>")
        << QStringLiteral("<QList>") << QStringLiteral("<QMap>")
        << QStringLiteral("<QSet>") << QStringLiteral("<QStringList>")
        << QStringLiteral("<QByteArray>") << QStringLiteral("<QDateTime>")
        << QStringLiteral("<QMetaType>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx.m_out, fileName, additionalIncludes);

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
                    if (f.m_type.dynamicCast<Parser::SetType>()) {
                        auto t = f.m_type.dynamicCast<Parser::SetType>();
                        typeName = getIdentifier(t->m_valueType);
                    }
                    else if (f.m_type.dynamicCast<Parser::ListType>()) {
                        auto t = f.m_type.dynamicCast<Parser::ListType>();
                        typeName = getIdentifier(t->m_valueType);
                    }
                    else if (f.m_type.dynamicCast<Parser::MapType>()) {
                        auto t = f.m_type.dynamicCast<Parser::MapType>();
                        typeName = getIdentifier(t->m_valueType);
                        typeName2 = getIdentifier(t->m_keyType);
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
                << ": public EvernoteException"
                << endl << "{" << endl
                << "public:" << endl;

            for(const auto & f : s.m_fields) {
                ctx.m_out << "    " << fieldToStr(f) << ";" << endl;
            }

            ctx.m_out << endl;
            ctx.m_out << "    " << s.m_name
                << "();" << endl;
            ctx.m_out << "    virtual ~" << s.m_name
                << "() throw() Q_DECL_OVERRIDE;" << endl;

            if (!s.m_fields.isEmpty()) {
                ctx.m_out << endl;
                ctx.m_out << "    " << s.m_name
                    << "(const " << s.m_name
                    << " & other);" << endl;
            }

            ctx.m_out << "    const char * what() const throw() "
                << "Q_DECL_OVERRIDE;" << endl;
            ctx.m_out << "    virtual QSharedPointer<"
                << "EverCloudExceptionData> exceptionData() "
                << "const Q_DECL_OVERRIDE;" << endl;
        }
        else
        {
            ctx.m_out << "struct QEVERCLOUD_EXPORT "
                << s.m_name << " {" << endl;
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

                ctx.m_out << "    " << fieldToStr(f) << ";" << endl;
            }
        }

        ctx.m_out << endl;
        ctx.m_out << QString::fromUtf8(
            "    bool operator==(const %1 & other) const").arg(s.m_name) << endl;
        ctx.m_out << "    {" << endl;

        bool first = true;
        for(const auto & f : s.m_fields)
        {
            if(first) {
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
        ctx.m_out << "    }" << endl << endl;

        ctx.m_out << "};" << endl << endl;
    }
    ctx.m_out << endl << endl;

    QStringList extraLinesOutsideNamespace;
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
        << QStringLiteral("Types_io.h") << QStringLiteral("<Helpers.h>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx.m_out, QStringLiteral("Types.h"), additionalIncludes);

    ctx.m_out << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations)
    {
        ctx.m_out <<  "void readEnum" << e.m_name
            << "(ThriftBinaryBufferReader & r, "
            << e.m_name << " & e) {" << endl;

        ctx.m_out << "    qint32 i;" << endl;
        ctx.m_out << "    r.readI32(i);" << endl;
        ctx.m_out << "    switch(i) {" << endl;

        for(const auto & v : e.m_values) {
            QString value = e.m_name + QStringLiteral("::") + v.first;
            ctx.m_out << "    case static_cast<int>("
                << value << "): e = " << value
                << "; break;" << endl;
        }

        ctx.m_out << "    default: throw ThriftException(ThriftException::Type::"
            "INVALID_DATA, QStringLiteral(\"Incorrect value for enum "
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

    for(const auto & s: qAsConst(structsAndExceptions))
    {
        if (exceptions.contains(s.m_name))
        {
            ctx.m_out << s.m_name << "::" << s.m_name
                << "() {}" << endl;
            ctx.m_out << s.m_name << "::~" << s.m_name
                << "() throw() {}" << endl;

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
            << "(ThriftBinaryBufferWriter & w, const "
            << s.m_name << " & s) {" << endl;

        ctx.m_out << "    w.writeStructBegin(QStringLiteral(\""
            << s.m_name  << "\"));" << endl;

        writeThriftWriteFields(
            ctx.m_out, s.m_fields, s.m_name, QStringLiteral("s."));

        ctx.m_out << "    w.writeFieldStop();" << endl;
        ctx.m_out << "    w.writeStructEnd();" << endl;
        ctx.m_out << "}" << endl << endl;

        ctx.m_out << "void read" << s.m_name
            << "(ThriftBinaryBufferReader & r, "
            << s.m_name << " & s) {" << endl;
        ctx.m_out << "    QString fname;" << endl;
        ctx.m_out << "    ThriftFieldType::type fieldType;" << endl;
        ctx.m_out << "    qint16 fieldId;" << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    bool " << field.m_name
                    << "_isset = false;" << endl;
            }
        }

        ctx.m_out << "    r.readStructBegin(fname);" << endl;
        ctx.m_out << "    while(true)" << endl
            << "    {" << endl;
        ctx.m_out << "        r.readFieldBegin(fname, fieldType, "
            << "fieldId);" << endl;
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

            ctx.m_out << "            } else {" << endl;
            ctx.m_out << "                r.skip(fieldType);" << endl;
            ctx.m_out << "            }" << endl;
            ctx.m_out << "        } else" << endl;
        }

        ctx.m_out << "        {" << endl;
        ctx.m_out << "            r.skip(fieldType);" << endl;
        ctx.m_out << "        }" << endl;
        ctx.m_out << "        r.readFieldEnd();" << endl;
        ctx.m_out << "    }" << endl;
        ctx.m_out << "    r.readStructEnd();" << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                ctx.m_out << "    if(!" << field.m_name
                    << "_isset) throw ThriftException("
                    << "ThriftException::Type::INVALID_DATA, "
                    << "QStringLiteral(\""
                    << s.m_name << "." << field.m_name
                    << " has no value\"));"
                    << endl;
            }
        }
        ctx.m_out << "}" << endl << endl;
    }
    ctx.m_out << endl;

    ctx.m_out << "/** @endcond */" << endl << endl;
    ctx.m_out << endl << endl;

    writeBodyFooter(ctx.m_out);
}

void Generator::generateServicesHeader(Parser * parser, const QString & outPath)
{
    const QString fileName = QStringLiteral("Services.h");
    OutputFileContext ctx(fileName, outPath, OutputFileType::Interface);

    QStringList additionalIncludes = QStringList()
        << QStringLiteral("../AsyncResult.h")
        << QStringLiteral("../RequestContext.h")
        << QStringLiteral("../Optional.h")
        << QStringLiteral("Constants.h")
        << QStringLiteral("Types.h")
        << QStringLiteral("<QObject>");
    sortIncludes(additionalIncludes);

    writeHeaderHeader(ctx.m_out, fileName, additionalIncludes);

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

            ctx.m_out << "        IRequestContextPtr ctx = {}";
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
            ctx.m_out << "I" << s.m_name << " * new" << s.m_name << "(" << endl;

            if (s.m_name == QStringLiteral("UserStore")) {
                ctx.m_out << "    QString host," << endl
                    << "    IRequestContextPtr ctx = {}," << endl
                    << "    QObject * parent = nullptr);" << endl
                    << endl;
            }
            else {
                ctx.m_out << "    QString noteStoreUrl = QString()," << endl
                    << "    IRequestContextPtr ctx = {}," << endl
                    << "    QObject * parent = nullptr);" << endl
                    << endl;
            }
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
        << QStringLiteral("Types_io.h") << QStringLiteral("<Helpers.h>")
        << QStringLiteral("<algorithm>") << QStringLiteral("<cmath>");
    sortIncludes(additionalIncludes);

    writeHeaderBody(ctx.m_out, QStringLiteral("Services.h"), additionalIncludes);

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

    ctx.m_out << blockSeparator << endl << endl;
    generateDurableServiceCommonCode(ctx);

    for(const auto & s: services) {
        ctx.m_out << blockSeparator << endl << endl;
        generateDurableServiceClassDefinition(s, ctx);
    }

    ctx.m_out << blockSeparator << endl << endl;

    for(const auto & s: services)
    {
        ctx.m_out << "I" << s.m_name << " * new" << s.m_name << "(" << endl;

        if (s.m_name == QStringLiteral("UserStore")) {
            ctx.m_out << "    QString host," << endl
                << "    IRequestContextPtr ctx," << endl
                << "    QObject * parent)" << endl
                << "{" << endl
                << "    return new " << s.m_name
                << "(host, ctx, parent);" << endl
                << "}" << endl
                << endl;
        }
        else {
            ctx.m_out << "    QString noteStoreUrl," << endl
                << "    IRequestContextPtr ctx," << endl
                << "    QObject * parent)" << endl
                << "{" << endl
                << "    return new " << s.m_name
                << "(noteStoreUrl, ctx, parent);" << endl
                << "}" << endl
                << endl;
        }
    }

    writeBodyFooter(ctx.m_out);

    ctx.m_out << endl;
    ctx.m_out << "#include <Services.moc>" << endl;
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

    ctx.m_out << "class Q_DECL_HIDDEN " << className
        << ": public I" << service.m_name << endl << "{" << endl;
    ctx.m_out << "    Q_OBJECT" << endl;
    ctx.m_out << "    Q_DISABLE_COPY(" << className << ")" << endl;
    ctx.m_out << "public:" << endl;

    if (service.m_name == QStringLiteral("UserStore"))
    {
        ctx.m_out << "    explicit " << className << "(" << endl;

        if (serviceClassType == ServiceClassType::NonDurable) {
            ctx.m_out << "            QString host," << endl;
        }
        else {
            ctx.m_out << "            I" << service.m_name << "Ptr service,"
                << endl;
        }

        ctx.m_out << "            IRequestContextPtr ctx = {}," << endl
            << "            QObject * parent = nullptr) :" << endl
            << "        IUserStore(parent)," << endl;

        if (serviceClassType == ServiceClassType::Durable) {
            ctx.m_out << "        m_service(std::move(service))," << endl;
        }

        ctx.m_out << "        m_ctx(std::move(ctx))" << endl
            << "    {" << endl
            << "        if (!m_ctx) {" << endl
            << "            m_ctx = newRequestContext();" << endl
            << "        }" << endl << endl;

        if (serviceClassType == ServiceClassType::NonDurable) {
            ctx.m_out << "        QUrl url;" << endl
                << "        url.setScheme(QStringLiteral(\"https\"));" << endl
                << "        url.setHost(host);" << endl
                << "        url.setPath(QStringLiteral(\"/edam/user\"));" << endl
                << "        m_url = url.toString(QUrl::StripTrailingSlash);"
                << endl;
        }

        ctx.m_out << "    }" << endl << endl;
    }
    else
    {
        ctx.m_out << "    explicit " << className << "(" << endl;

        if (serviceClassType == ServiceClassType::NonDurable) {
            ctx.m_out << "            QString noteStoreUrl = {}," << endl;
        }
        else {
            ctx.m_out << "            I" << service.m_name << "Ptr service,"
                << endl;
        }

        ctx.m_out << "            IRequestContextPtr ctx = {}," << endl
            << "            QObject * parent = nullptr) :" << endl
            << "        INoteStore(parent)," << endl;

        if (serviceClassType == ServiceClassType::NonDurable) {
            ctx.m_out << "        m_url(std::move(noteStoreUrl))," << endl;
        }
        else {
            ctx.m_out << "        m_service(std::move(service))," << endl;
        }

        ctx.m_out << "        m_ctx(std::move(ctx))" << endl
            << "    {" << endl
            << "        if (!m_ctx) {" << endl
            << "            m_ctx = newRequestContext();" << endl
            << "        }" << endl
            << "    }" << endl
            << endl;

        if (serviceClassType == ServiceClassType::NonDurable)
        {
            ctx.m_out << "    explicit " << className
                << "(QObject * parent) :" << endl
                << "        INoteStore(parent)" << endl
                << "    {" << endl
                << "        m_ctx = newRequestContext();" << endl
                << "    }" << endl
                << endl;

            ctx.m_out << "    void setNoteStoreUrl(QString noteStoreUrl)" << endl
                << "    {" << endl
                << "        m_url = std::move(noteStoreUrl);" << endl
                << "    }"
                << endl << endl;

            ctx.m_out << "    QString noteStoreUrl()" << endl
                << "    {" << endl
                << "        return m_url;" << endl
                << "    }"
                << endl << endl;
        }
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

        ctx.m_out << "        IRequestContextPtr ctx = {}";
        ctx.m_out << ") Q_DECL_OVERRIDE;" << endl << endl;

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
        ctx.m_out << ") Q_DECL_OVERRIDE;" << endl << endl;
    }

    ctx.m_out << "private:" << endl;

    if (serviceClassType == ServiceClassType::NonDurable) {
        ctx.m_out << "    QString m_url;" << endl;
    }
    else {
        ctx.m_out << "    I" << service.m_name << "Ptr m_service;" << endl;
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

        QString prepareParamsName = service.m_name + QStringLiteral("_") +
            f.m_name + QStringLiteral("_prepareParams");

        QString readReplyName = service.m_name + QStringLiteral("_") + f.m_name +
            QStringLiteral("_readReply");

        int lastId = f.m_params.last().m_id;

        bool isVoidResult =
            !f.m_type.dynamicCast<Parser::VoidType>().isNull();

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
        ctx.m_out << "    ThriftBinaryBufferWriter w;" << endl;
        ctx.m_out << "    qint32 cseqid = 0;" << endl;
        ctx.m_out << "    w.writeMessageBegin(" << endl;
        ctx.m_out << "        QStringLiteral(\"" << f.m_name
            << "\"), ThriftMessageType::T_CALL, cseqid);" << endl;
        ctx.m_out << "    w.writeStructBegin(" << endl;
        ctx.m_out << "        QStringLiteral(\"" << service.m_name
            << "_" << f.m_name << "_pargs\"));" << endl;

        writeThriftWriteFields(
            ctx.m_out, f.m_params, f.m_name, QLatin1Literal(""));

        ctx.m_out << "    w.writeFieldStop();" << endl;
        ctx.m_out << "    w.writeStructEnd();" << endl;
        ctx.m_out << "    w.writeMessageEnd();" << endl;
        ctx.m_out << "    return w.buffer();" << endl;
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

        ctx.m_out << "    ThriftBinaryBufferReader r(reply);" << endl
            << "    qint32 rseqid = 0;" << endl
            << "    QString fname;" << endl
            << "    ThriftMessageType::type mtype;" << endl
            << "    r.readMessageBegin(fname, mtype, rseqid);" << endl
            << "    if (mtype == ThriftMessageType::T_EXCEPTION) {" << endl
            << "        ThriftException e = readThriftException(r);" << endl
            << "        r.readMessageEnd();" << endl
            << "        throw e;" << endl
            << "    }" << endl
            << "    if (mtype != ThriftMessageType::T_REPLY) {" << endl
            << "        r.skip(ThriftFieldType::T_STRUCT);" << endl
            << "        r.readMessageEnd();" << endl
            << "        throw ThriftException(ThriftException::Type::"
            << "INVALID_MESSAGE_TYPE);" << endl
            << "    }" << endl
            << "    if (fname.compare(QStringLiteral(\"" << f.m_name
            << "\")) != 0) {" << endl
            << "        r.skip(ThriftFieldType::T_STRUCT);" << endl
            << "        r.readMessageEnd();" << endl
            << "        throw ThriftException(ThriftException::Type::"
            << "WRONG_METHOD_NAME);" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    ThriftFieldType::type fieldType;" << endl
            << "    qint16 fieldId;" << endl
            << "    r.readStructBegin(fname);" << endl
            << "    while(true) {" << endl
            << "        r.readFieldBegin(fname, fieldType, fieldId);"
            << endl
            << "        if (fieldType == ThriftFieldType::T_STOP) break;"
            << endl;

        if (!isVoidResult)
        {
            Parser::Field result;
            result.m_id = 0;
            result.m_name = QStringLiteral("result");
            result.m_required = Parser::Field::RequiredFlag::Required;
            result.m_type = f.m_type;

            ctx.m_out << "        if (fieldId == 0) {" << endl
                << "            if (fieldType == "
                << typeToStr(
                    f.m_type, f.m_name, MethodType::ThriftFieldType)
                << ") {" << endl
                << "                resultIsSet = true;" << endl;

            writeThriftReadField(
                ctx.m_out, result, f.m_name + QStringLiteral("."),
                QLatin1Literal(""));

            ctx.m_out << "            } else {" << endl
                << "                r.skip(fieldType);" << endl
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
                ctx.m_out << "       else ";
            }

            ctx.m_out << "if (fieldId == "  << th.m_id
                << ") {" << endl;

            QString exceptionType = typeToStr(
                th.m_type, f.m_name + QStringLiteral(", ") + th.m_name);

            ctx.m_out << "            if (fieldType == ThriftFieldType::"
                << "T_STRUCT) {" << endl
                << "                " << exceptionType << " e;" << endl
                << "                read" << exceptionType << "(r, e);"
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
                << "                r.skip(fieldType);" << endl
                << "            }" << endl
                << "        }" << endl;
        }

        ctx.m_out << "        else {" << endl
            << "            r.skip(fieldType);" << endl
            << "        }" << endl
            << "        r.readFieldEnd();" << endl
            << "    }" << endl
            << "    r.readStructEnd();" << endl;

        ctx.m_out << "    r.readMessageEnd();" << endl;

        if (!isVoidResult) {
            ctx.m_out << "    if (!resultIsSet) {" << endl
                << "        throw ThriftException(" << endl
                << "            ThriftException::Type::"
                << "MISSING_RESULT," << endl
                << "            QStringLiteral(\""
                << f.m_name << ": missing result\"));" << endl
                << "    }" << endl
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
            << "        ctx = m_ctx;" << endl
            << "    }" << endl;

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
        ctx.m_out << ");" << endl;

        ctx.m_out << "    QByteArray reply = askEvernote(m_url, params);"
            << endl;
        if (isVoidResult) {
            ctx.m_out << "    " << readReplyName << "(reply);" << endl;
        }
        else {
            ctx.m_out << "    return " << readReplyName << "(reply);"
                << endl;
        }

        ctx.m_out << "}" << endl << endl;

        ctx.m_out << "AsyncResult* " << service.m_name << "::" << f.m_name
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

        ctx.m_out << "    if (!ctx) {" << endl
            << "        ctx = m_ctx;" << endl
            << "    }" << endl;

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
        ctx.m_out << ");" << endl
            << "    return new AsyncResult(m_url, params, "
            << asyncReadFunctionName << ");" << endl;

        ctx.m_out << "}" << endl << endl;
    }
}

void Generator::generateDurableServiceCommonCode(OutputFileContext & ctx)
{
    ctx.m_out << "struct RetryState" << endl
        << "{" << endl
        << "    const quint64 m_started = QDateTime::currentMSecsSinceEpoch();"
        << endl
        << "    quint32 m_retryCount = 0;" << endl
        << "};" << endl << endl;

    ctx.m_out << "template <class T>" << endl
        << "struct RequestState" << endl
        << "{" << endl
        << "    T m_request;" << endl
        << "    AsyncResult * m_response;" << endl << endl
        << "    RequestState(T && request, AsyncResult * response) :" << endl
        << "        m_request(std::move(request))," << endl
        << "        m_response(response)" << endl
        << "    {}" << endl
        << "};" << endl << endl;

    ctx.m_out << "quint64 exponentiallyIncreasedTimeoutMsec("
        << "quint64 timeout, const quint64 maxTimeout)" << endl
        << "{" << endl
        << "    timeout = static_cast<quint64>(std::floor(timeout * 1.6 + 0.5));"
        << endl
        << "    timeout = std::min(timeout, maxTimeout);" << endl
        << "    return timeout;" << endl
        << "}" << endl << endl;
}

void Generator::generateDurableServiceClassDefinition(
    const Parser::Service & service, OutputFileContext & ctx)
{
    for(const auto & func: qAsConst(service.m_functions))
    {
        if (func.m_isOneway) {
            throw std::runtime_error("oneway functions are not supported");
        }

        ctx.m_out << typeToStr(func.m_type, func.m_name) << " "
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
            << "        ctx = m_ctx;" << endl
            << "    }" << endl << endl;

        ctx.m_out << "    RetryState state;" << endl;
        ctx.m_out << "    state.m_retryCount = ctx->maxRequestRetryCount();"
            << endl;
        ctx.m_out << "    while(state.m_retryCount)" << endl
            << "    {" << endl
            << "        try" << endl
            << "        {" << endl;

        ctx.m_out << "            ";
        bool isVoidResult =
            !func.m_type.dynamicCast<Parser::VoidType>().isNull();
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

        if (!isVoidResult) {
            ctx.m_out << "            return res;" << endl;
        }

        ctx.m_out << "        }" << endl;

        ctx.m_out << "        catch(...)" << endl
            << "        {" << endl
            << "            --state.m_retryCount;" << endl
            << "            if (!state.m_retryCount) {" << endl
            << "                throw;" << endl
            << "            }" << endl << endl
            << "            if (ctx->increaseRequestTimeoutExponentially()) {"
            << endl
            << "                quint64 maxRequestTimeout = ctx->maxRequestTimeout();"
            << endl
            << "                quint64 timeout = exponentiallyIncreasedTimeoutMsec("
            << endl
            << "                    ctx->requestTimeout()," << endl
            << "                    maxRequestTimeout);" << endl
            << "                ctx = newRequestContext(" << endl
            << "                    ctx->authenticationToken()," << endl
            << "                    timeout," << endl
            << "                    /* increase request timeout exponentially = */ true,"
            << endl
            << "                    maxRequestTimeout," << endl
            << "                    ctx->maxRequestRetryCount());" << endl
            << "            }" << endl
            << "        }" << endl
            << "    }" << endl << endl
            << "    throw EverCloudException(\"no retry attempts left\");" << endl
            << "}" << endl << endl;

        // TODO: implement version with AsyncResult
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
        auto casted = t.m_type.dynamicCast<Parser::BaseType>();
        if (!casted.isNull()) {
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
}

