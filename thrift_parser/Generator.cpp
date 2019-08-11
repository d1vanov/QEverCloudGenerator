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

static const char * disclaimer =
    "/**\n"
    " * Original work: Copyright (c) 2014 Sergey Skoblikov\n"
    " * Modified work: Copyright (c) 2015-2019 Dmitry Ivanov\n"
    " *\n"
    " * This file is a part of QEverCloud project and is distributed under\n"
    " * the terms of MIT license:\n"
    " * https://opensource.org/licenses/MIT\n"
    " *\n"
    " * This file was generated from Evernote Thrift API\n"
    " */\n";

QString Generator::generatedHeaderOutputPath(const QString & outPath)
{
    QString headerOutPath = outPath + QStringLiteral("/headers/generated");

    QDir headerOutDir(headerOutPath);
    if (!headerOutDir.exists())
    {
        bool res = headerOutDir.mkpath(headerOutDir.absolutePath());
        if (Q_UNLIKELY(!res)) {
            throw std::runtime_error(
                QString("Can't create output directory for header files: %1")
                .arg(headerOutDir.absolutePath()).toStdString());
        }
    }

    return headerOutPath;
}

QString Generator::generatedSourceOutputPath(const QString & outPath)
{
    QString sourceOutPath = outPath + QStringLiteral("/src/generated");

    QDir sourceOutDir(sourceOutPath);
    if (!sourceOutDir.exists())
    {
        bool res = sourceOutDir.mkpath(sourceOutDir.absolutePath());
        if (Q_UNLIKELY(!res)) {
            throw std::runtime_error(
                QString("Can't create output directory for source files: %1")
                .arg(sourceOutDir.absolutePath()).toStdString());
        }
    }

    return sourceOutPath;
}

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

void Generator::writeHeaderHeader(
    QTextStream & out, const QString & fileName,
    const QStringList & additionalPreIncludes,
    const QStringList & additionalPostIncludes)
{
    out << disclaimer << endl;
    out << endl;

    QString guard =
        QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(fileName.split('.')[0].toUpper());
    out << "#ifndef " << guard << endl;
    out << "#define " << guard << endl;
    out << endl;

    if (fileName != QStringLiteral("EDAMErrorCode.h"))
    {
        if (fileName != QStringLiteral("types_impl.h")) {
            out << "#include \"../Optional.h\"" << endl;
            out << "#include \"../export.h\"" << endl;
        }
        else {
            out << "#include <Optional.h>" << endl;
        }

        for(const auto & include: qAsConst(additionalPreIncludes))
        {
            if (include.startsWith(QChar::fromLatin1('<'))) {
                out << "#include " << include << endl;
            }
            else {
                out << "#include \"" << include << "\"" << endl;
            }
        }

        auto includes = QStringList()
            << "QMap" << "QList" << "QSet" << "QString" << "QStringList"
            << "QByteArray" << "QDateTime" << "QMetaType";
        for(const auto & include: qAsConst(includes)) {
            out << "#include <" << include << ">" << endl;
        }

        for(const auto & include: qAsConst(additionalPostIncludes))
        {
            if (include.startsWith(QChar::fromLatin1('<'))) {
                out << "#include " << include << endl;
            }
            else {
                out << "#include \"" << include << "\"" << endl;
            }
        }

        out << endl;
    }
    else
    {
        out << "#include \"../export.h\"" << endl;
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
    out << endl;
    out << "#include <generated/" << headerFileName << ">" << endl;
    out << "#include \"../impl.h\"" << endl;

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
}

void Generator::writeHeaderFooter(
    QTextStream & out, const QString & fileName,
    const QStringList & extraContent)
{
    out << "} // namespace qevercloud" << endl;

    for(const auto & line: extraContent)
    {
        if (!line.isEmpty()) {
            out << line << endl;
        }
    }

    QString guard = QString("QEVERCLOUD_GENERATED_%1_H").arg(fileName.split('.')[0].toUpper());
    out << endl;
    out << "#endif // " << guard << endl;
}

void Generator::writeBodyFooter(QTextStream & out)
{
    out << endl;
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
                    strm << "const " << typeName << "&";
                }
            }
        }
        else if (methodType == MethodType::TypeName)
        {
            result = nameOfType;
            if (m_allEnums.contains(result)) {
                result = result + QStringLiteral("::type");
            }
        }
        else if(methodType == MethodType::ReadTypeName)
        {
            result = (nameOfType == QStringLiteral("Timestamp")
                      ? QStringLiteral("qint64")
                      : nameOfType);
            if (m_allEnums.contains(result)) {
                result = result + QStringLiteral("::type");
            }
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
                        result = "";
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
                strm << "QMap< " << typeToStr(mapType->m_keyType, identifier)
                    << ", " << typeToStr(mapType->m_valueType, identifier)
                    << " >";
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
                strm << "QSet< " << typeToStr(setType->m_valueType, identifier)
                    << " >";
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
                    strm << "QList< " << valueType << " >";
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
    QSharedPointer<Parser::Type> type, const QString & identifier)
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
        for(const auto & v: qAsConst(listValue->m_values)) {
            QTextStream strm(&result, QIODevice::Append);
            strm << " << " << valueToStr(
                v, QSharedPointer<Parser::Type>(nullptr), identifier);
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

void Generator::generateConstants(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = QStringLiteral("constants.h");

    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated header file for writing: %1")
            .arg(headerFile.fileName()).toStdString());
    }

    // Generate header:

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    writeHeaderHeader(hout, headerFileName);

    QString fileName;
    const auto & constants = parser->constants();
    for(const auto & c: constants)
    {
        if (fileName != c.m_fileName)
        {
            bool first = fileName.isEmpty();
            if (!first) {
                hout << endl;
            }

            fileName = c.m_fileName;
            hout << "// " << c.m_fileName << endl;
        }

        if (!c.m_docComment.isEmpty()) {
            hout << c.m_docComment << endl;
        }

        hout << "QEVERCLOUD_EXPORT extern const "
            << typeToStr(c.m_type, c.m_name)
            << " " << c.m_name << ";" << endl << endl;
    }

    writeHeaderFooter(hout, headerFileName);

    // Generate source:

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString bodyFileName = "constants.cpp";

    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString(
            "Can't open the generated source file for writing: %1")
            .arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    QStringList additionalPreIncludes;
    additionalPreIncludes << QStringLiteral("<Helpers.h>");
    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

    fileName = QLatin1Literal("");
    for(const auto & c: constants)
    {
        if (fileName != c.m_fileName) {
            fileName = c.m_fileName;
            bout << endl << "// " << c.m_fileName << endl << endl;
        }

        if (c.m_value.isNull()) {
            throw std::runtime_error(
                QString("Constant without a value: %1")
                .arg(c.m_name).toStdString());
        }

        bout << "const " << typeToStr(c.m_type, c.m_name) << " "
             << c.m_name << " = " << valueToStr(c.m_value, c.m_type, c.m_name)
             << ";" << endl;
    }

    writeBodyFooter(bout);
}

QString Generator::fieldToStr(const Parser::Field & field)
{
   QString s = typeToStr(field.m_type, field.m_name);
   if (field.m_required == Parser::Field::RequiredFlag::Optional) {
       s = "Optional< " + s + " >";
   }

   s += " " + field.m_name;
   if (field.m_initializer) {
       s += " = " + valueToStr(field.m_initializer, field.m_type, field.m_name);
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
            out << QStringLiteral("    if (") << fieldPrefix
                << field.m_name << QStringLiteral(".isSet()) {") << endl;
        }

        out << ident
            << QStringLiteral("    w.writeFieldBegin(QStringLiteral(\"")
            << field.m_name << QStringLiteral("\"), ")
            << typeToStr(
                field.m_type, identPrefix + QStringLiteral(". ") + field.m_name,
                MethodType::ThriftFieldType)
            << QStringLiteral(", ") << field.m_id << QStringLiteral(");")
            << endl;

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

            out << ident << QStringLiteral("    w.writeListBegin(")
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker
                << QStringLiteral(".length());") << endl;

            out << ident
                << QStringLiteral("    for(const auto & value: qAsConst(")
                << fieldMoniker << QStringLiteral(")) {") << endl;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << QStringLiteral("        ") << writeMethod
                << QStringLiteral("value")
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeListEnd();") << endl;
        }
        else if (writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            QSharedPointer<Parser::Type> valueType =
                field.m_type.dynamicCast<Parser::SetType>()->m_valueType;

            out << ident << QStringLiteral("    w.writeSetBegin(")
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker
                << QStringLiteral(".count());") << endl;

            out << ident
                << QStringLiteral("    for(const auto & value: qAsConst(")
                << fieldMoniker << ")) {" << endl;

            QString writeMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << QStringLiteral("        ") << writeMethod
                << QStringLiteral("value")
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeSetEnd();") << endl;
        }
        else if (writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            QSharedPointer<Parser::Type> keyType =
                field.m_type.dynamicCast<Parser::MapType>()->m_keyType;

            QSharedPointer<Parser::Type> valueType =
                field.m_type.dynamicCast<Parser::MapType>()->m_valueType;

            out << ident << QStringLiteral("    w.writeMapBegin(")
                << typeToStr(
                    keyType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << QStringLiteral(", ")
                << typeToStr(
                    valueType, identPrefix + QStringLiteral(",") + field.m_name,
                    MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker
                << QStringLiteral(".size());") << endl;

            out << ident
                << QStringLiteral("    for(auto it = ")
                << fieldMoniker << QStringLiteral(".constBegin(), end = ")
                << fieldMoniker
                << QStringLiteral(".constEnd(); it != end; ++it) {") << endl;

            QString keyWriteMethod = typeToStr(
                keyType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            QString valueWriteMethod = typeToStr(
                valueType, identPrefix + QStringLiteral(",") + field.m_name,
                MethodType::WriteMethod);

            out << ident << QStringLiteral("        ") << keyWriteMethod
                << QStringLiteral("it.key()")
                << (keyWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << QStringLiteral(");") << endl;
            out << ident << QStringLiteral("        ") << valueWriteMethod
                << QStringLiteral("it.value()")
                << (valueWriteMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeMapEnd();") << endl;
        }
        else
        {
            out << ident << QStringLiteral("    ") << writeMethod
                << fieldMoniker
                << (writeMethod.contains(QStringLiteral("static_cast<"))
                    ? QStringLiteral(")")
                    : QLatin1Literal(""))
                << QStringLiteral(");") << endl;
        }

        out << ident << QStringLiteral("    w.writeFieldEnd();") << endl;
        if (isOptional) {
            out << QStringLiteral("    }") << endl;
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
        << QStringLiteral(" v;") << endl;

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

        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;")
            << endl;
        out << indent << QStringLiteral("r.readListBegin(elemType, size);")
            << endl;
        out << indent << QStringLiteral("v.reserve(size);") << endl;
        out << indent << QStringLiteral("if(elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::")
            << QStringLiteral("INVALID_DATA, QStringLiteral(\"Incorrect list ")
            << QStringLiteral("type (")
            << identPrefix + field.m_name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {")
            << endl;
        out << indent << QStringLiteral("    ")
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << QStringLiteral(" elem;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod
            << QStringLiteral("elem);") << endl;
        out << indent << QStringLiteral("    v.append(elem);") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readListEnd();") << endl;
    }
    else if (readMethod.contains("readSetBegin"))
    {
        QSharedPointer<Parser::Type> valueType =
            field.m_type.dynamicCast<Parser::SetType>()->m_valueType;

        QString valueReadMethod = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ReadMethod);

        QString valueThriftType = typeToStr(
            valueType, identPrefix + field.m_name, MethodType::ThriftFieldType);

        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;") << endl;
        out << indent << QStringLiteral("r.readSetBegin(elemType, size);") << endl;
        out << indent << QStringLiteral("v.reserve(size);") << endl;
        out << indent << QStringLiteral("if (elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::")
            << QStringLiteral("INVALID_DATA, QStringLiteral(\"Incorrect set ")
            << QStringLiteral("type (")
            << identPrefix + field.m_name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {")
            << endl;
        out << indent << QStringLiteral("    ")
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << QStringLiteral(" elem;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod
            << QStringLiteral("elem);") << endl;
        out << indent << QStringLiteral("    v.insert(elem);") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readSetEnd();") << endl;
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

        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type keyType;")
            << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;")
            << endl;
        out << indent
            << QStringLiteral("r.readMapBegin(keyType, elemType, size);")
            << endl;
        out << indent << QStringLiteral("if (keyType != ") << keyThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::")
            << QStringLiteral("INVALID_DATA, QStringLiteral(\"Incorrect map ")
            << QStringLiteral("key type (")
            << identPrefix << field.m_name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("if (elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::")
            << QStringLiteral("INVALID_DATA, QStringLiteral(\"Incorrect map ")
            << QStringLiteral("value type (")
            << identPrefix + field.m_name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {")
            << endl;
        out << indent << QStringLiteral("    ")
            << typeToStr(
                keyType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << QStringLiteral(" key;") << endl;
        out << indent << QStringLiteral("    ") << keyReadMethod
            << QStringLiteral("key);") << endl;
        out << indent << QStringLiteral("    ")
            << typeToStr(
                valueType, identPrefix + field.m_name, MethodType::ReadTypeName)
            << QStringLiteral(" value;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod
            << QStringLiteral("value);") << endl;
        out << indent << QStringLiteral("    v[key] = value;") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readMapEnd();") << endl;
    }
    else
    {
        out << indent << readMethod << QStringLiteral("v);") << endl;
    }

    out << indent << fieldParent << field.m_name << QStringLiteral(" = v;")
        << endl;
}

void Generator::generateTypes(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = QStringLiteral("types.h");
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated header file for writing: %1")
            .arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    const QString EDAMErrorCodeHeaderFileName =
        QStringLiteral("EDAMErrorCode.h");

    QStringList additionalPreIncludes =
        QStringList() << EDAMErrorCodeHeaderFileName;

    QStringList additionalPostIncludes = QStringList()
        << QStringLiteral("<QSharedPointer>") << QStringLiteral("<QMetaType>");

    writeHeaderHeader(
        hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    QFile EDAMErrorCodeHeaderFile(
        QDir(headerOutPath).absoluteFilePath(EDAMErrorCodeHeaderFileName));
    if (!EDAMErrorCodeHeaderFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated header file for writing: %1")
            .arg(EDAMErrorCodeHeaderFile.fileName()).toStdString());
    }

    QTextStream houtEDAMErrorCode(&EDAMErrorCodeHeaderFile);
    houtEDAMErrorCode.setCodec("UTF-8");

    writeHeaderHeader(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    const auto & enumerations = parser->enumerations();
    for(const auto & e: enumerations)
    {
        if (e.m_name == QStringLiteral("EDAMErrorCode"))
        {
            if (!e.m_docComment.isEmpty()) {
                houtEDAMErrorCode << e.m_docComment << endl;
            }

            houtEDAMErrorCode << QStringLiteral("struct QEVERCLOUD_EXPORT ")
                << e.m_name << endl << QStringLiteral("{") << endl;

            houtEDAMErrorCode << QStringLiteral("    enum type") << endl
                << QStringLiteral("    {") << endl;

            for(int i = 0; i< e.m_values.length(); i++)
            {
                const QPair<QString, QString>& v = e.m_values[i];
                houtEDAMErrorCode << QStringLiteral("        ") << v.first;

                if (!v.second.isEmpty()) {
                    houtEDAMErrorCode << QStringLiteral(" = ") << v.second;
                }

                if (i < (e.m_values.length() - 1)) {
                    houtEDAMErrorCode << QStringLiteral(",");
                }

                houtEDAMErrorCode << endl;
            }

            houtEDAMErrorCode << QStringLiteral("    };") << endl;
            houtEDAMErrorCode << QStringLiteral("};") << endl << endl;
        }
        else
        {
            if (!e.m_docComment.isEmpty()) {
                hout << e.m_docComment << endl;
            }

            hout << QStringLiteral("struct QEVERCLOUD_EXPORT ")
                << e.m_name << QStringLiteral(" {") << endl;
            hout << QStringLiteral("    enum type {") << endl;

            for(int i = 0; i< e.m_values.length(); i++)
            {
                const QPair<QString, QString> & v = e.m_values[i];
                hout << "        " << v.first;

                if (!v.second.isEmpty()) {
                    hout << QStringLiteral(" = ") << v.second;
                }

                if (i < (e.m_values.length() - 1)) {
                    hout << QStringLiteral(",");
                }

                hout << endl;
            }

            hout << QStringLiteral("    };") << endl;
            hout << QStringLiteral("};") << endl << endl;
        }
    }

    hout << endl;
    houtEDAMErrorCode << endl;

    const auto & typedefs = parser->typedefs();
    for(const auto & t: typedefs)
    {
        if (!t.m_docComment.isEmpty()) {
            hout << t.m_docComment << endl;
        }

        hout << QStringLiteral("typedef ") << typeToStr(t.m_type, t.m_name)
             << QStringLiteral(" ") << t.m_name << QStringLiteral(";")
             << endl << endl;
    }
    hout << endl;

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
            hout << s.m_docComment << endl;
        }
        else {
            hout << QStringLiteral("/** NO DOC COMMENT ID FOUND */") << endl;
        }

        if (exceptions.contains(s.m_name))
        {
            hout << QStringLiteral("class QEVERCLOUD_EXPORT ") << s.m_name
                << QStringLiteral(": public EvernoteException")
                << endl << QStringLiteral("{") << endl
                << QStringLiteral("public:") << endl;

            for(const auto & f : s.m_fields) {
                hout << QStringLiteral("    ") << fieldToStr(f)
                    << QStringLiteral(";") << endl;
            }

            hout << endl;
            hout << QStringLiteral("    ") << s.m_name
                << QStringLiteral("();") << endl;
            hout << QStringLiteral("    virtual ~") << s.m_name
                << QStringLiteral("() throw() Q_DECL_OVERRIDE;") << endl;

            if (!s.m_fields.isEmpty()) {
                hout << endl;
                hout << QStringLiteral("    ") << s.m_name
                    << QStringLiteral("(const ") << s.m_name
                    << QStringLiteral(" & other);") << endl;
            }

            hout << QStringLiteral("    const char * what() const throw() ")
                << QStringLiteral("Q_DECL_OVERRIDE;") << endl;
            hout << QStringLiteral("    virtual QSharedPointer<")
                << QStringLiteral("EverCloudExceptionData> exceptionData() ")
                << QStringLiteral("const Q_DECL_OVERRIDE;") << endl;
        }
        else
        {
            hout << QStringLiteral("struct QEVERCLOUD_EXPORT ")
                << s.m_name << QStringLiteral(" {") << endl;
            for(const auto & f: qAsConst(s.m_fields))
            {
                if (s.m_fieldComments.contains(f.m_name))
                {
                    QStringList lines =
                        s.m_fieldComments[f.m_name].split(QStringLiteral("\n"));
                    for(const auto & line: lines) {
                        hout << QStringLiteral("    ") << line << endl;
                    }
                }
                else
                {
                    hout << QStringLiteral("    /** NOT DOCUMENTED */") << endl;
                }

                hout << QStringLiteral("    ") << fieldToStr(f)
                    << QStringLiteral(";") << endl;
            }
        }

        hout << endl;
        hout << QString::fromUtf8(
            "    bool operator==(const %1 & other) const").arg(s.m_name) << endl;
        hout << QStringLiteral("    {") << endl;

        bool first = true;
        for(const auto & f : s.m_fields)
        {
            if(first) {
                first = false;
                hout << QStringLiteral("        return ");
            }
            else {
                hout << QStringLiteral("            && ");
            }

            if (f.m_required == Parser::Field::RequiredFlag::Optional) {
                hout << QStringLiteral("%1.isEqual(other.%1)").arg(f.m_name)
                    << endl;
            }
            else {
                hout << QStringLiteral("(%1 == other.%1)").arg(f.m_name)
                    << endl;
            }
        }

        hout << QStringLiteral("        ;") << endl << QStringLiteral("    }")
            << endl << endl;
        hout << QString::fromUtf8(
            "    bool operator!=(const %1 & other) const").arg(s.m_name)
            << endl;
        hout << QStringLiteral("    {") << endl;
        hout << QStringLiteral("        return !(*this == other);") << endl;
        hout << QStringLiteral("    }") << endl << endl;

        hout << QStringLiteral("};") << endl << endl;
    }
    hout << endl;

    writeHeaderFooter(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    hout << endl;
    hout << QStringLiteral("} // namespace qevercloud") << endl<< endl;

    for(const auto & s: qAsConst(ordered)) {
        hout << QStringLiteral("Q_DECLARE_METATYPE(qevercloud::") << s.m_name
            << QStringLiteral(")") << endl;
    }
    hout << endl;

    QString guard = QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H")
        .arg(headerFileName.split(QStringLiteral("."))[0].toUpper());
    hout << QStringLiteral("#endif // ") << guard << endl;

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString typesImplHeaderFileName = QStringLiteral("types_impl.h");
    QFile typesImplHeaderFile(
        QDir(sourceOutPath).absoluteFilePath(typesImplHeaderFileName));
    if (!typesImplHeaderFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated header file for writing: %1")
            .arg(typesImplHeaderFile.fileName()).toStdString());
    }

    QTextStream hout2(&typesImplHeaderFile);
    hout2.setCodec("UTF-8");

    additionalPreIncludes.clear();
    additionalPreIncludes << QStringLiteral("<generated/types.h>")
                          << QStringLiteral("../impl.h");

    writeHeaderHeader(hout2, typesImplHeaderFileName, additionalPreIncludes);

    hout2 << QStringLiteral("/** @cond HIDDEN_SYMBOLS  */") << endl << endl;

    QList<Parser::Structure> structuresAndExceptions = parser->structures();
    structuresAndExceptions << parser->exceptions();

    for(const auto & s: qAsConst(structuresAndExceptions)) {
        hout2 << QStringLiteral("void write") << s.m_name
            << QStringLiteral("(ThriftBinaryBufferWriter & w, const ")
            << s.m_name << QStringLiteral(" & s);") << endl;
        hout2 << QStringLiteral("void read") << s.m_name
            << QStringLiteral("(ThriftBinaryBufferReader & r, ")
            << s.m_name << QStringLiteral(" & s);") << endl;
    }
    hout2 << endl;

    for(const auto & e: enumerations) {
        hout2 << QStringLiteral("void readEnum") << e.m_name
            << QStringLiteral("(ThriftBinaryBufferReader & r, ")
            << e.m_name << QStringLiteral("::type & e);") << endl;
    }
    hout2 << endl;
    hout2 << QStringLiteral("/** @endcond */") << endl;

    writeHeaderFooter(hout2, typesImplHeaderFileName);

    const QString bodyFileName = QStringLiteral("types.cpp");
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated source file for writing: %1")
            .arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    additionalPreIncludes.clear();
    additionalPreIncludes << QStringLiteral("../impl.h")
                          << QStringLiteral("types_impl.h")
                          << QStringLiteral("<qt4helpers.h>");

    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

    bout << QStringLiteral("/** @cond HIDDEN_SYMBOLS  */") << endl << endl;

    for(const auto & e: enumerations)
    {
        bout <<  QStringLiteral("void readEnum") << e.m_name
            << QStringLiteral("(ThriftBinaryBufferReader & r, ")
            << e.m_name << QStringLiteral("::type & e) {") << endl;

        bout << QStringLiteral("    qint32 i;") << endl;
        bout << QStringLiteral("    r.readI32(i);") << endl;
        bout << QStringLiteral("    switch(i) {") << endl;

        for(const auto & v : e.m_values) {
            QString value = e.m_name + QStringLiteral("::") + v.first;
            bout << QStringLiteral("    case static_cast<int>(")
                << value << QStringLiteral("): e = ") << value
                << QStringLiteral("; break;") << endl;
        }

        bout << QStringLiteral(
            "    default: throw ThriftException(ThriftException::Type::"
            "INVALID_DATA, QStringLiteral(\"Incorrect value for enum ")
             << e.m_name << QStringLiteral("\"));") << endl;
        bout << QStringLiteral("    }") << endl;
        bout << QStringLiteral("}") << endl << endl;
    }

    for(const auto & s: qAsConst(structuresAndExceptions))
    {
        if (exceptions.contains(s.m_name))
        {
            bout << s.m_name << QStringLiteral("::") << s.m_name
                << QStringLiteral("() {}") << endl;
            bout << s.m_name << QStringLiteral("::~") << s.m_name
                << QStringLiteral("() throw() {}") << endl;

            if (!s.m_fields.isEmpty())
            {
                bout << s.m_name << QStringLiteral("::") << s.m_name
                    << QStringLiteral("(const ") << s.m_name
                    << QStringLiteral("& other) : EvernoteException(other)") << endl;
                bout << QStringLiteral("{") << endl;
                for(const auto & f : s.m_fields) {
                    bout << QStringLiteral("   ") << f.m_name
                        << QStringLiteral(" = other.") << f.m_name
                        << QStringLiteral(";") << endl;
                }
                bout << QStringLiteral("}") << endl;
            }
        }

        bout << QStringLiteral("void write") << s.m_name
            << QStringLiteral("(ThriftBinaryBufferWriter & w, const ")
            << s.m_name << QStringLiteral(" & s) {") << endl;

        bout << QStringLiteral("    w.writeStructBegin(QStringLiteral(\"")
            << s.m_name  << QStringLiteral("\"));") << endl;
        writeThriftWriteFields(bout, s.m_fields, s.m_name, QStringLiteral("s."));
        bout << QStringLiteral("    w.writeFieldStop();") << endl;
        bout << QStringLiteral("    w.writeStructEnd();") << endl;
        bout << QStringLiteral("}") << endl << endl;

        bout << QStringLiteral("void read") << s.m_name
            << QStringLiteral("(ThriftBinaryBufferReader & r, ")
            << s.m_name << QStringLiteral(" & s) {") << endl;
        bout << QStringLiteral("    QString fname;") << endl;
        bout << QStringLiteral("    ThriftFieldType::type fieldType;") << endl;
        bout << QStringLiteral("    qint16 fieldId;") << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                bout << QStringLiteral("    bool ") << field.m_name
                    << QStringLiteral("_isset = false;") << endl;
            }
        }

        bout << QStringLiteral("    r.readStructBegin(fname);") << endl;
        bout << QStringLiteral("    while(true)") << endl
            << QStringLiteral("    {") << endl;
        bout << QStringLiteral("        r.readFieldBegin(fname, fieldType, ")
            << QStringLiteral("fieldId);") << endl;
        bout << QStringLiteral("        if (fieldType == ")
            << QStringLiteral("ThriftFieldType::T_STOP) break;") << endl;

        for(const auto & field : s.m_fields)
        {
            bool isOptional =
                (field.m_required == Parser::Field::RequiredFlag::Optional);
            bout << QStringLiteral("        if (fieldId == ") << field.m_id
                << QStringLiteral(") {") << endl;
            bout << QStringLiteral("            if (fieldType == ")
                 << typeToStr(
                     field.m_type, s.m_name + "." + field.m_name,
                     MethodType::ThriftFieldType)
                 << QStringLiteral(") {") << endl;

            if (!isOptional) {
                bout << QStringLiteral("                ") << field.m_name
                    << QStringLiteral("_isset = true;") << endl;
            }

            writeThriftReadField(
                bout, field, s.m_name + QStringLiteral("."),
                QStringLiteral("s."));

            bout << QStringLiteral("            } else {") << endl;
            bout << QStringLiteral("                r.skip(fieldType);")
                << endl;
            bout << QStringLiteral("            }") << endl;
            bout << QStringLiteral("        } else") << endl;
        }

        bout << QStringLiteral("        {") << endl;
        bout << QStringLiteral("            r.skip(fieldType);") << endl;
        bout << QStringLiteral("        }") << endl;
        bout << QStringLiteral("        r.readFieldEnd();") << endl;
        bout << QStringLiteral("    }") << endl;
        bout << QStringLiteral("    r.readStructEnd();") << endl;

        for(const auto & field : s.m_fields)
        {
            if (field.m_required != Parser::Field::RequiredFlag::Optional) {
                bout << QStringLiteral("    if(!") << field.m_name
                     << QStringLiteral("_isset) throw ThriftException(")
                     << QStringLiteral("ThriftException::Type::INVALID_DATA, ")
                     << QStringLiteral("QStringLiteral(\"")
                     << s.m_name << QStringLiteral(".") << field.m_name
                     << QStringLiteral(" has no value\"));")
                     << endl;
            }
        }
        bout << QStringLiteral("}") << endl << endl;
    }
    bout << endl;

    bout << QStringLiteral("/** @endcond */") << endl << endl;
    bout << endl;

    writeBodyFooter(bout);
}

void Generator::generateServices(Parser * parser, const QString & outPath)
{
    // Generate header

    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = QStringLiteral("services.h");

    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if(!headerFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated header file for writing: %1")
            .arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    QStringList additionalPreIncludes = QStringList()
        << QStringLiteral("../AsyncResult.h") << QStringLiteral("constants.h")
        << QStringLiteral("types.h");

    QStringList additionalPostIncludes = QStringList()
        << QStringLiteral("<QObject>");

    writeHeaderHeader(
        hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    const auto & services = parser->services();
    for(const auto & s: services)
    {
        if (!s.m_extends.isEmpty()) {
            throw std::runtime_error("extending services is not supported");
        }

        if (!s.m_docComment.isEmpty()) {
            hout << s.m_docComment << endl;
        }

        hout << "class QEVERCLOUD_EXPORT " << s.m_name << ": public QObject"
            << endl << "{" << endl;
        hout << "    Q_OBJECT" << endl;
        hout << "    Q_DISABLE_COPY(" << s.m_name << ")" << endl;
        hout << "public:" << endl;

        if (s.m_name == QStringLiteral("UserStore"))
        {
            hout << "    explicit UserStore(QString host, "
                << "QString authenticationToken = QString(), "
                << "QObject * parent = nullptr);"
                << endl << endl;
        }
        else
        {
            hout << "    explicit NoteStore("
                << "QString noteStoreUrl = QString(), "
                << "QString authenticationToken = QString(), "
                << "QObject * parent = nullptr);" << endl;

            hout << "    explicit NoteStore(QObject * parent);" << endl << endl;
            hout << "    void setNoteStoreUrl(QString noteStoreUrl)"
                << " { m_url = noteStoreUrl; }" << endl;
            hout << "    QString noteStoreUrl() { return m_url; }" << endl
                << endl;
        }

        hout << "    void setAuthenticationToken(QString authenticationToken) "
            << "{ m_authenticationToken = authenticationToken; }" << endl;
        hout << "    QString authenticationToken() "
            << "{ return m_authenticationToken; }" << endl << endl;

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
                    hout << "    " << line << endl;
                }
            }

            hout << "    " << typeToStr(func.m_type, func.m_name) << " "
                 << func.m_name << "(";
            int lastId = func.m_params.last().m_id;
            bool tokenParamIsPresent = false;
            for(const auto & param: qAsConst(func.m_params))
            {
                if (param.m_name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(
                        param.m_type,
                        func.m_name + QStringLiteral(", ") + param.m_name,
                        MethodType::FuncParamType);
                    hout << " " << param.m_name;
                    if (param.m_initializer) {
                        hout << " = " << valueToStr(
                            param.m_initializer, param.m_type,
                            func.m_name + QStringLiteral(", ") + param.m_name);
                    }

                    if (param.m_id != lastId || tokenParamIsPresent) {
                        hout << ", ";
                    }
                }
            }

            if (tokenParamIsPresent) {
                hout << "QString authenticationToken = QString()";
            }
            hout << ");" << endl << endl;

            hout << "    /** Asynchronous version of @link " << func.m_name
                << " @endlink */" << endl;
            hout << "    AsyncResult * " << func.m_name << "Async(";
            tokenParamIsPresent = false;
            for(const auto & param: qAsConst(func.m_params))
            {
                if (param.m_name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(
                        param.m_type,
                        func.m_name + QStringLiteral(", ") + param.m_name,
                        MethodType::FuncParamType);
                    hout << " " << param.m_name;
                    if (param.m_initializer) {
                        hout << " = " << valueToStr(
                            param.m_initializer,
                            param.m_type,
                            func.m_name + QStringLiteral(", ") + param.m_name);
                    }

                    if (param.m_id != lastId || tokenParamIsPresent) {
                        hout << ", ";
                    }
                }
            }

            if (tokenParamIsPresent) {
                hout << "QString authenticationToken = QString()";
            }
            hout << ");" << endl << endl;
        }

        hout << "private:" << endl;
        hout << "    QString m_url;" << endl;
        hout << "    QString m_authenticationToken;" << endl;
        hout << "};" << endl << endl;
    }

    QStringList metatypeDeclarations;
    metatypeDeclarations.reserve(6);
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::Notebook >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::Tag >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::SavedSearch >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::NoteVersionId >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::SharedNotebook >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::LinkedNotebook >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::BusinessInvitation >)");
    metatypeDeclarations
        << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::UserProfile >)");

    writeHeaderFooter(hout, headerFileName, metatypeDeclarations);

    // Generate source

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString bodyFileName = QStringLiteral("services.cpp");

    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8(
            "Can't open the generated source file for writing: %1")
            .arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    additionalPreIncludes.clear();
    additionalPreIncludes << QStringLiteral("../impl.h")
        << QStringLiteral("types_impl.h")
        << QStringLiteral("<Helpers.h>");
    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

    for(const auto & s: services)
    {
        for(const auto & f: s.m_functions)
        {
            QString prepareParamsName = s.m_name + QStringLiteral("_") +
                f.m_name + QStringLiteral("_prepareParams");

            QString readReplyName = s.m_name + QStringLiteral("_") + f.m_name +
                QStringLiteral("_readReply");

            int lastId = f.m_params.last().m_id;

            bool isVoidResult =
                !f.m_type.dynamicCast<Parser::VoidType>().isNull();

            bout << "QByteArray " << prepareParamsName << "(";
            for(const auto & param: f.m_params)
            {
                bout << typeToStr(
                    param.m_type,
                    f.m_name + QStringLiteral(", ") + param.m_name,
                    MethodType::FuncParamType);
                bout << " " << param.m_name;
                if (param.m_id != lastId) {
                    bout << ", ";
                }
            }

            bout << ")" << endl;
            bout << "{" << endl;
            bout << "    ThriftBinaryBufferWriter w;" << endl;
            bout << "    qint32 cseqid = 0;" << endl;
            bout << "    w.writeMessageBegin(QStringLiteral(\"" << f.m_name
                 << "\"), ThriftMessageType::T_CALL, cseqid);" << endl;
            bout << "    w.writeStructBegin(QStringLiteral(\"" << s.m_name
                 << "_" << f.m_name << "_pargs\"));" << endl;

            writeThriftWriteFields(
                bout, f.m_params, f.m_name, QLatin1Literal(""));

            bout << "    w.writeFieldStop();" << endl;
            bout << "    w.writeStructEnd();" << endl;
            bout << "    w.writeMessageEnd();" << endl;
            bout << "    return w.buffer();" << endl;
            bout << "}" << endl << endl;

            bout << (isVoidResult
                     ? QStringLiteral("void")
                     : typeToStr(f.m_type, f.m_name))
                 << " " << readReplyName << "(QByteArray reply)" << endl;
            bout << "{" << endl;

            if (!isVoidResult) {
                bout << "    bool resultIsSet = false;" << endl;
                bout << "    " << typeToStr(f.m_type, f.m_name)
                    << " result = " << typeToStr(f.m_type, f.m_name)
                    << "();" << endl;
            }

            bout << "    ThriftBinaryBufferReader r(reply);" << endl;
            bout << "    qint32 rseqid = 0;" << endl;
            bout << "    QString fname;" << endl;
            bout << "    ThriftMessageType::type mtype;" << endl;
            bout << "    r.readMessageBegin(fname, mtype, rseqid);" << endl;
            bout << "    if (mtype == ThriftMessageType::T_EXCEPTION) {" << endl;
            bout << "      ThriftException e = readThriftException(r);" << endl;
            bout << "      r.readMessageEnd();" << endl;
            bout << "      throw e;" << endl;
            bout << "    }" << endl;
            bout << "    if (mtype != ThriftMessageType::T_REPLY) {" << endl;
            bout << "      r.skip(ThriftFieldType::T_STRUCT);" << endl;
            bout << "      r.readMessageEnd();" << endl;
            bout << "      throw ThriftException(ThriftException::Type::"
                << "INVALID_MESSAGE_TYPE);" << endl;
            bout << "    }" << endl;
            bout << "    if (fname.compare(QStringLiteral(\"" << f.m_name
                << "\")) != 0) {" << endl;
            bout << "      r.skip(ThriftFieldType::T_STRUCT);" << endl;
            bout << "      r.readMessageEnd();" << endl;
            bout << "      throw ThriftException(ThriftException::Type::"
                << "WRONG_METHOD_NAME);" << endl;
            bout << "    }" << endl << endl;

            bout << "    ThriftFieldType::type fieldType;" << endl;
            bout << "    qint16 fieldId;" << endl;
            bout << "    r.readStructBegin(fname);" << endl;
            bout << "    while(true) {" << endl;
            bout << "        r.readFieldBegin(fname, fieldType, fieldId);"
                << endl;
            bout << "        if(fieldType == ThriftFieldType::T_STOP) break;"
                << endl;

            if (!isVoidResult)
            {
                Parser::Field result;
                result.m_id = 0;
                result.m_name = QStringLiteral("result");
                result.m_required = Parser::Field::RequiredFlag::Required;
                result.m_type = f.m_type;
                bout << "        if(fieldId == 0) {" << endl;
                bout << "            if(fieldType == "
                    << typeToStr(
                        f.m_type, f.m_name, MethodType::ThriftFieldType)
                     << ") {" << endl;
                bout << "                resultIsSet = true;" << endl;

                writeThriftReadField(
                    bout, result, f.m_name + QStringLiteral("."),
                    QLatin1Literal(""));

                bout << "            } else {" << endl;
                bout << "                r.skip(fieldType);" << endl;
                bout << "            }" << endl;
                bout << "        }" << endl;
            }

            bool firstThrow = isVoidResult;
            for(const auto & th: f.m_throws)
            {
                if (firstThrow) {
                    firstThrow = false;
                    bout << "        ";
                }
                else {
                    bout << "       else ";
                }

                bout << "if (fieldId == "  << th.m_id
                    << ") {" << endl;

                QString exceptionType = typeToStr(
                    th.m_type, f.m_name + QStringLiteral(", ") + th.m_name);

                bout << "            if (fieldType == ThriftFieldType::"
                    << "T_STRUCT) {" << endl;
                bout << "                " << exceptionType << " e;" << endl;
                bout << "                read" << exceptionType << "(r, e);"
                    << endl;

                if (exceptionType == QStringLiteral("EDAMSystemException")) {
                    bout << "                throwEDAMSystemException(e);"
                        << endl;
                }
                else {
                    bout << "                throw e;" << endl;
                }

                bout << "            }" << endl;
                bout << "            else {" << endl;
                bout << "                r.skip(fieldType);" << endl;
                bout << "            }" << endl;
                bout << "        }" << endl;
            }

            bout << "        else {" << endl;
            bout << "            r.skip(fieldType);" << endl;
            bout << "        }" << endl;
            bout << "        r.readFieldEnd();" << endl;
            bout << "    }" << endl;
            bout << "    r.readStructEnd();" << endl;

            bout << "    r.readMessageEnd();" << endl;

            if (!isVoidResult) {
                bout << "    if (!resultIsSet) {" << endl;
                bout << "        throw ThriftException(ThriftException::Type::"
                    << "MISSING_RESULT, QStringLiteral(\""
                    << f.m_name << ": missing result\"));" << endl;
                bout << "    }" << endl;
                bout << "    return result;" << endl;
            }

            bout << "}" << endl << endl;

            QString asyncReadFunctionName =
                readReplyName + QStringLiteral("Async");
            bout << "QVariant " << asyncReadFunctionName << "(QByteArray reply)"
                << endl;
            bout << "{" << endl;
            if (isVoidResult) {
                bout << "    " << readReplyName << "(reply);" << endl;
                bout << "    return QVariant();" << endl;
            }
            else {
                bout << "    return QVariant::fromValue(" << readReplyName
                    << "(reply));" << endl;
            }
            bout << "}" << endl << endl;

            bout << typeToStr(f.m_type, f.m_name) << " "
                << s.m_name << "::" << f.m_name << "(";
            bool tokenParamIsPresent = false;
            for(const auto & param : f.m_params)
            {
                if (param.m_name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    bout << typeToStr(
                        param.m_type, f.m_name + ", " + param.m_name,
                        MethodType::FuncParamType);
                    bout << " " << param.m_name;
                    if (param.m_id != lastId || tokenParamIsPresent) {
                        bout << ", ";
                    }
                }
            }

            if (tokenParamIsPresent) {
                bout << "QString authenticationToken";
            }
            bout << ")" << endl;
            bout << "{" << endl;

            if (tokenParamIsPresent) {
                bout << "    if (authenticationToken.isEmpty()) {" << endl;
                bout << "        authenticationToken = m_authenticationToken;"
                    << endl;
                bout << "    }" << endl;
            }

            bout << "    QByteArray params = " << prepareParamsName << "(";
            for(const auto & param : f.m_params) {
                bout << param.m_name;
                if (param.m_id != lastId) {
                    bout << ", ";
                }
            }
            bout << ");" << endl;

            bout << "    QByteArray reply = askEvernote(m_url, params);" << endl;
            if (isVoidResult) {
                bout << "    " << readReplyName << "(reply);" << endl;
            }
            else {
                bout << "    return " << readReplyName << "(reply);" << endl;
            }

            bout << "}" << endl << endl;


            bout << "AsyncResult* " << s.m_name << "::" << f.m_name << "Async(";
            tokenParamIsPresent = false;
            for(const auto & param : f.m_params)
            {
                if (param.m_name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    bout << typeToStr(
                        param.m_type,
                        f.m_name + QStringLiteral(", ") + param.m_name,
                        MethodType::FuncParamType);
                    bout << " " << param.m_name;

                    if (param.m_id != lastId || tokenParamIsPresent) {
                        bout << ", ";
                    }
                }
            }

            if (tokenParamIsPresent) {
                bout << "QString authenticationToken";
            }
            bout << ")" << endl;
            bout << "{" << endl;

            if (tokenParamIsPresent) {
                bout << "    if (authenticationToken.isEmpty()) {" << endl;
                bout << "        authenticationToken = m_authenticationToken;"
                    << endl;
                bout << "    }" << endl;
            }

            bout << "    QByteArray params = " << prepareParamsName << "(";
            for(const auto & param: f.m_params)
            {
                bout << param.m_name;
                if (param.m_id != lastId) {
                    bout << ", ";
                }
            }
            bout << ");" << endl;
            bout << "    return new AsyncResult(m_url, params, "
                << asyncReadFunctionName << ");" << endl;

            bout << "}" << endl << endl;
        }
    }

    writeBodyFooter(bout);
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

    generateConstants(parser, outPath);
    generateTypes(parser, outPath);
    generateServices(parser, outPath);
}

