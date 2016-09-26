#include "Generator.h"
#include "../qt4helpers.h"
#include <QDir>
#include <QFile>
#include <QMap>
#include <QString>

static const char * disclaimer = "/**\n"
                                 " * Original work: Copyright (c) 2014 Sergey Skoblikov\n"
                                 " * Modified work: Copyright (c) 2015-2016 Dmitry Ivanov\n"
                                 " *\n"
                                 " * This file is a part of QEverCloud project and is distributed under the terms of MIT license:\n"
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
            throw std::runtime_error(QString("Can't create output directory for header files: %1").arg(headerOutDir.absolutePath()).toStdString());
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
            throw std::runtime_error(QString("Can't create output directory for source files: %1").arg(sourceOutDir.absolutePath()).toStdString());
        }
    }

    return sourceOutPath;
}

QString Generator::clearInclude(QString s)
{
    for(auto it = includeList_.constBegin(), end = includeList_.constEnd(); it != end; ++it)
    {
        const QString & inc = *it;
        if (s.startsWith(inc)) {
            return s.mid(inc.length());
        }
    }

    return s;
}

QString Generator::clearTypedef(QString s)
{
    if (typedefMap_.contains(s)) {
        return typedefMap_.value(s);
    }

    return s;
}

void Generator::writeHeaderHeader(QTextStream & out, QString fileName,
                                  QStringList additionalPreIncludes,
                                  QStringList additionalPostIncludes)
{
    out << disclaimer << endl;
    out << endl;
    QString guard = QString("QEVERCLOUD_GENERATED_%1_H").arg(fileName.split('.')[0].toUpper());
    out << "#ifndef " << guard << endl;
    out << "#define " << guard << endl;
    out << endl;

    if (fileName != "EDAMErrorCode.h")
    {
        if (fileName != "types_impl.h") {
            out << "#include \"../Optional.h\"" << endl;
            out << "#include \"../export.h\"" << endl;
        }
        else {
            out << "#include <Optional.h>" << endl;
        }

        for(auto it = additionalPreIncludes.constBegin(), end = additionalPreIncludes.constEnd(); it != end; ++it)
        {
            const QString & include = *it;
            if(include.startsWith('<')) {
                out << "#include " << include << endl;
            }
            else {
                out << "#include \"" << include << "\"" << endl;
            }
        }

        QStringList includes;
        includes << "QMap" << "QList" << "QSet" << "QString" << "QStringList"
                 << "QByteArray" << "QDateTime" << "QMetaType";
        for(auto it = includes.constBegin(), end = includes.constEnd(); it != end; ++it) {
            out << "#include <" << *it << ">" << endl;
        }

        for(auto it = additionalPostIncludes.constBegin(), end = additionalPostIncludes.constEnd(); it != end; ++it)
        {
            const QString & include = *it;
            if(include.startsWith('<')) {
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

void Generator::writeHeaderFooter(QTextStream & out, QString fileName, QStringList extraContent)
{
    out << "} // namespace qevercloud" << endl;

    for(auto it = extraContent.constBegin(), end = extraContent.constEnd(); it != end; ++it)
    {
        const QString & extraContentLine = *it;
        if (!extraContentLine.isEmpty()) {
            out << extraContentLine << endl;
        }
    }

    QString guard = QString("QEVERCLOUD_GENERATED_%1_H").arg(fileName.split('.')[0].toUpper());
    out << endl;
    out << "#endif // " << guard << endl;
}

void Generator::writeHeaderBody(QTextStream& out, QString headerFileName, QStringList moreIncludes)
{
    out << disclaimer << endl;
    out << endl;
    out << "#include <generated/" << headerFileName << ">" << endl;
    out << "#include \"../impl.h\"" << endl;

    for(auto it = moreIncludes.constBegin(), end = moreIncludes.constEnd(); it != end; ++it)
    {
        const QString & include = *it;
        if(include.startsWith('<')) {
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

void Generator::writeBodyFooter(QTextStream & out)
{
    out << endl;
    out << "} // namespace qevercloud" << endl;
}

QString Generator::typeToStr(QSharedPointer<Parser::Type> type, QString identifier, MethodType methodType)
{
    QSharedPointer<Parser::BaseType>        basetype        = type.dynamicCast<Parser::BaseType>();
    QSharedPointer<Parser::VoidType>        voidtype        = type.dynamicCast<Parser::VoidType>();
    QSharedPointer<Parser::IdentifierType>  identifiertype  = type.dynamicCast<Parser::IdentifierType>();
    QSharedPointer<Parser::MapType>         maptype         = type.dynamicCast<Parser::MapType>();
    QSharedPointer<Parser::SetType>         settype         = type.dynamicCast<Parser::SetType>();
    QSharedPointer<Parser::ListType>        listtype        = type.dynamicCast<Parser::ListType>();
    QString result;

    QString justTypeName;
    if (methodType == MethodType::FuncParamType) {
        justTypeName = typeToStr(type, identifier, MethodType::TypeName);
    }

    if (!basetype.isNull())
    {
        if (basetype->basetype == "bool")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "bool";
                break;
            case MethodType::WriteMethod:
                result = "w.writeBool(";
                break;
            case MethodType::ReadMethod:
                result = "r.readBool(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_BOOL";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default: result = "";
            }
        }
        else if (basetype->basetype == "string")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "QString";
                break;
            case MethodType::WriteMethod:
                result = "w.writeString(";
                break;
            case MethodType::ReadMethod:
                result = "r.readString(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_STRING";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "double")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "double";
                break;
            case MethodType::WriteMethod:
                result = "w.writeDouble(";
                break;
            case MethodType::ReadMethod:
                result = "r.readDouble(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_DOUBLE";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "binary")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "QByteArray";
                break;
            case MethodType::WriteMethod:
                result = "w.writeBinary(";
                break;
            case MethodType::ReadMethod:
                result = "r.readBinary(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_STRING";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "byte")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "quint8";
                break;
            case MethodType::WriteMethod:
                result = "w.writeByte(";
                break;
            case MethodType::ReadMethod:
                result = "r.readByte(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_BYTE";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "i16")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "qint16";
                break;
            case MethodType::WriteMethod:
                result = "w.writeI16(";
                break;
            case MethodType::ReadMethod:
                result = "r.readI16(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_I16";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "i32")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "qint32";
                break;
            case MethodType::WriteMethod:
                result = "w.writeI32(";
                break;
            case MethodType::ReadMethod:
                result = "r.readI32(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_I32";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
        else if (basetype->basetype == "i64")
        {
            switch(methodType)
            {
            case MethodType::TypeName:
            case MethodType::ReadTypeName:
                result = "qint64";
                break;
            case MethodType::WriteMethod:
                result = "w.writeI64(";
                break;
            case MethodType::ReadMethod:
                result = "r.readI64(";
                break;
            case MethodType::ThriftFieldType:
                result = "ThriftFieldType::T_I64";
                break;
            case MethodType::FuncParamType:
                result = justTypeName;
                break;
            default:
                result = "";
            }
        }
    }
    else if (voidtype)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = "void";
            break;
        default: result = "";
        }
    }
    else if (identifiertype)
    {
        QString nameOfType = clearInclude(identifiertype->identifier);
        if (methodType == MethodType::FuncParamType)
        {
            if (allenums_.contains(nameOfType))
            {
                result = justTypeName;
            }
            else
            {
                QString nameOfType2 = clearTypedef(nameOfType);
                if (nameOfType2 != nameOfType) {
                    result = justTypeName;
                }
                else {
                    result = "const " + justTypeName + "&";
                }
            }
        }
        else if (methodType == MethodType::TypeName)
        {
            result = nameOfType;
            if (allenums_.contains(result)) {
                result = result + "::type";
            }
        }
        else if(methodType == MethodType::ReadTypeName)
        {
            result = nameOfType == "Timestamp" ? "qint64" : nameOfType;
            if (allenums_.contains(result)) {
                result = result + "::type";
            }
        }
        else
        {
            QString nameOfType2 = clearTypedef(nameOfType);
            if (nameOfType2 != nameOfType)
            {
                if (!baseTypes_.contains(nameOfType2)) {
                    throw std::runtime_error("typedefs are supported for base types only");
                }

                QSharedPointer<Parser::BaseType> type2(new Parser::BaseType);
                type2->basetype = nameOfType2;
                result = typeToStr(type2, identifier, methodType);
            }
            else
            {
                if (allstructs_.contains(nameOfType) || allexceptions_.contains(nameOfType))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        result = "write" + nameOfType + "(w, ";
                        break;
                    case MethodType::ReadMethod:
                        result = "read" + nameOfType + "(r, ";
                        break;
                    case MethodType::ThriftFieldType:
                        result = "ThriftFieldType::T_STRUCT";
                        break;
                    default:
                        result = "";
                    }
                }
                else if (allenums_.contains(nameOfType))
                {
                    switch(methodType)
                    {
                    case MethodType::WriteMethod:
                        result = "w.writeI32(static_cast<qint32>(";
                        break;
                    case MethodType::ReadMethod:
                        result = "readEnum" + nameOfType + "(r, ";
                        break;
                    case MethodType::ThriftFieldType:
                        result = "ThriftFieldType::T_I32";
                        break;
                    default:
                        result = "";
                    }
                }
            }
        }
    }
    else if (maptype)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = "QMap< " + typeToStr(maptype->keyType, identifier) + ", " + typeToStr(maptype->valueType, identifier) + " >";
            break;
        case MethodType::WriteMethod:
            result = "w.writeMapBegin(";
            break;
        case MethodType::ReadMethod:
            result = "r.readMapBegin(";
            break;
        case MethodType::ThriftFieldType:
            result = "ThriftFieldType::T_MAP";
            break;
        case MethodType::FuncParamType:
            result = justTypeName;
            break;
        default:
            result = "";
        }
    }
    else if (settype)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            result = "QSet< " + typeToStr(settype->valueType, identifier) + " >";
            break;
        case MethodType::WriteMethod:
            result = "w.writeSetBegin(";
            break;
        case MethodType::ReadMethod:
            result = "r.readSetBegin(";
            break;
        case MethodType::ThriftFieldType:
            result = "ThriftFieldType::T_SET";
            break;
        case MethodType::FuncParamType:
            result = justTypeName;
            break;
        default:
            result = "";
        }
    }
    else if (listtype)
    {
        switch(methodType)
        {
        case MethodType::TypeName:
        case MethodType::ReadTypeName:
            {
                // list<string> => QStringList
                QString valueType = typeToStr(listtype->valueType, identifier);
                if (valueType == "QString") {
                    result = "QStringList";
                }
                else {
                    result = "QList< " + valueType + " >";
                }

                break;
            }
        case MethodType::WriteMethod:
            result = "w.writeListBegin(";
            break;
        case MethodType::ReadMethod:
            result = "r.readListBegin(";
            break;
        case MethodType::ThriftFieldType:
            result = "ThriftFieldType::T_LIST";
            break;
        case MethodType::FuncParamType:
            result = justTypeName;
            break;
        default:
            result = "";
        }
    }

    if (result.isEmpty() &&
        (methodType == MethodType::TypeName || methodType == MethodType::ReadTypeName ||
         methodType == MethodType::FuncParamType))
    {
        throw std::runtime_error(QString("Error! unrecognized type (%1)").arg(identifier).toStdString());
    }

    return result;
}

QString Generator::valueToStr(QSharedPointer<Parser::ConstValue> value, QSharedPointer<Parser::Type> type, QString identifier)
{
    if (value.isNull()) {
        return QString();
    }

    QSharedPointer<Parser::MapType> maptype = type.dynamicCast<Parser::MapType>();
    QSharedPointer<Parser::SetType> settype = type.dynamicCast<Parser::SetType>();
    QSharedPointer<Parser::ListType> listtype = type.dynamicCast<Parser::ListType>();

    QSharedPointer<Parser::LiteralValue> literalvalue = value.dynamicCast<Parser::LiteralValue>();
    QSharedPointer<Parser::ListValue> listvalue = value.dynamicCast<Parser::ListValue>();
    QSharedPointer<Parser::MapValue> mapvalue = value.dynamicCast<Parser::MapValue>();

    QString result;
    if (literalvalue)
    {
        result = literalvalue->value;
    }
    else if (listvalue)
    {
        if (!settype && !listtype) {
            throw std::runtime_error(QString("List initializer for a unsupported type for (%1)").arg(identifier).toStdString());
        }

        result = typeToStr(type, identifier) + "()";
        for(auto it = listvalue->values.constBegin(), end = listvalue->values.constEnd(); it != end; ++it) {
            const QSharedPointer<Parser::ConstValue> & v = *it;
            result += " << " + valueToStr(v, QSharedPointer<Parser::Type>(nullptr), identifier);
        }
    }
    else if (mapvalue) {
        throw std::runtime_error(QString("map constants are not implemented (%1)").arg(identifier).toStdString());
    }

    if (result.isEmpty()) {
        throw std::runtime_error(QString("Error! unrecognized constant value (%1)").arg(identifier).toStdString());
    }

    return result;
}

void Generator::generateConstants(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = "constants.h";
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
    }

    // Generate header:

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    writeHeaderHeader(hout, headerFileName);

    QString file;
    QList<Parser::Constant> constants = parser->constants();
    for(auto it = constants.constBegin(), end = constants.constEnd(); it != end; ++it)
    {
        const Parser::Constant & c = *it;

        if (file != c.file)
        {
            bool first = file.isEmpty();
            if (!first) {
                hout << endl;
            }

            file = c.file;
            hout << "// " << c.file << endl;
        }

        if (!c.docComment.isEmpty()) {
            hout << c.docComment << endl;
        }

        hout << "QEVERCLOUD_EXPORT extern const " << typeToStr(c.type, c.name)
             << " " << c.name << ";" << endl << endl;
    }

    writeHeaderFooter(hout, headerFileName);

    // Generate source:

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString bodyFileName = "constants.cpp";
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated source file for writing: %1").arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    writeHeaderBody(bout, headerFileName);

    file = "";
    for(auto it = constants.constBegin(), end = constants.constEnd(); it != end; ++it)
    {
        const Parser::Constant & c = *it;

        if (file != c.file) {
            file = c.file;
            bout << endl << "// " << c.file << endl << endl;
        }

        if (c.value.isNull()) {
            throw std::runtime_error(QString("Constant without a value: %1").arg(c.name).toStdString());
        }

        bout << "QEVERCLOUD_EXPORT const " << typeToStr(c.type, c.name) << " "
             << c.name << " = " << valueToStr(c.value, c.type, c.name) << ";" << endl;
    }

    writeBodyFooter(bout);
}

QString Generator::fieldToStr(const Parser::Field & field)
{
   QString s = typeToStr(field.type, field.name);
   if (field.required == Parser::Field::RequiredFlag::Optional) {
       s = "Optional< " + s + " >";
   }

   s += " " + field.name;
   if (field.initializer) {
       s += " = " + valueToStr(field.initializer, field.type, field.name);
   }

   return s;
}

QString Generator::getIdentifier(const QSharedPointer<Parser::Type> & type)
{
    auto it = type.dynamicCast<Parser::IdentifierType>();
    return (it
            ? clearInclude(it->identifier)
            : QString());
}

void Generator::writeThriftWriteFields(QTextStream & out, const QList<Parser::Field> & fields, QString identPrefix, QString fieldPrefix)
{
    for(auto it = fields.constBegin(), end = fields.constEnd(); it != end; ++it)
    {
        const Parser::Field & field = *it;

        QString ident = "";

        bool optional = (field.required == Parser::Field::RequiredFlag::Optional);
        if (optional) {
            ident = "    ";
            out << "    if(" << fieldPrefix + field.name << ".isSet()) {" << endl;
        }

        out << ident << "    w.writeFieldBegin(\"" << field.name << "\", "
            << typeToStr(field.type, identPrefix + ". " + field.name, MethodType::ThriftFieldType)
            << ", " << field.id << ");" << endl;

        QString fieldMoniker = fieldPrefix + field.name + (optional ? ".ref()" : "");
        QString writeMethod = typeToStr(field.type, identPrefix + "," + field.name, MethodType::WriteMethod);

        if (writeMethod.contains("writeListBegin"))
        {
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::ListType>()->valueType;
            out << ident << "    w.writeListBegin(" << typeToStr(valueType, identPrefix + "," + field.name, MethodType::ThriftFieldType)
                << ", " << fieldMoniker << ".length());" << endl;
            out << ident << "    for(" << typeToStr(field.type, QString(), MethodType::TypeName) << "::const_iterator it = "
                << fieldMoniker << ".constBegin(), end = " << fieldMoniker << ".constEnd(); it != end; ++it) {" << endl;
            QString writeMethod = typeToStr(valueType, identPrefix + "," + field.name, MethodType::WriteMethod);
            out << ident << "        " << writeMethod << "*it" << (writeMethod.contains("static_cast<") ? ")" : "")
                << ");" << endl;
            out << ident << "    }" << endl;
            out << ident << "    w.writeListEnd();" << endl;
        }
        else if(writeMethod.contains("writeSetBegin"))
        {
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::SetType>()->valueType;
            out << ident << "    w.writeSetBegin(" << typeToStr(valueType, identPrefix + "," + field.name, MethodType::ThriftFieldType)
                << ", " << fieldMoniker << ".count());" << endl;
            out << ident << "    for(" << typeToStr(field.type, QString(), MethodType::TypeName) << "::const_iterator it = "
                << fieldMoniker << ".constBegin(), end = " << fieldMoniker << ".constEnd(); it != end; ++it) {" << endl;
            QString writeMethod = typeToStr(valueType, identPrefix + "," + field.name, MethodType::WriteMethod);
            out << ident << "        " << writeMethod << "*it" << (writeMethod.contains("static_cast<") ? ")" : "")
                << ");" << endl;
            out << ident << "    }" << endl;
            out << ident << "    w.writeSetEnd();" << endl;
        }
        else if(writeMethod.contains("writeMapBegin"))
        {
            QSharedPointer<Parser::Type> keyType = field.type.dynamicCast<Parser::MapType>()->keyType;
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::MapType>()->valueType;
            out << ident << "    w.writeMapBegin(" << typeToStr(keyType, identPrefix + "," + field.name, MethodType::ThriftFieldType)
                << ", " << typeToStr(valueType, identPrefix + "," + field.name, MethodType::ThriftFieldType) << ", "
                << fieldMoniker << ".size());" << endl;
            out << ident << "    for(" << typeToStr(field.type, QString(), MethodType::TypeName) << "::const_iterator it = "
                << fieldMoniker << ".constBegin(), end = " << fieldMoniker << ".constEnd(); it != end; ++it) {" << endl;
            QString keyWriteMethod = typeToStr(keyType, identPrefix + "," + field.name, MethodType::WriteMethod);
            QString valueWriteMethod = typeToStr(valueType, identPrefix + "," + field.name, MethodType::WriteMethod);
            out << ident << "        " << keyWriteMethod << "it.key()" << (keyWriteMethod.contains("static_cast<") ? ")" : "")
                << ");" << endl;
            out << ident << "        " << valueWriteMethod << "it.value()" << (valueWriteMethod.contains("static_cast<") ? ")" : "")
                << ");" << endl;
            out << ident << "    }" << endl;
            out << ident << "    w.writeMapEnd();" << endl;
        }
        else
        {
            out << ident << "    " << writeMethod << fieldMoniker << (writeMethod.contains("static_cast<") ? ")" : "") << ");"
                << endl;
        }

        out << ident << "    w.writeFieldEnd();" << endl;
        if (optional) {
            out << "    }" << endl;
        }
    }
}

void Generator::writeThriftReadField(QTextStream & out, const Parser::Field & field, QString identPrefix, QString fieldParent)
{
    const char * indent = "                ";
    out << indent << typeToStr(field.type, identPrefix + field.name, MethodType::ReadTypeName) << " v;" << endl;
    QString readMethod = typeToStr(field.type, identPrefix + field.name, MethodType::ReadMethod);
    if (readMethod.contains("readListBegin"))
    {
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::ListType>()->valueType;
        QString valueReadMethod = typeToStr(valueType,  identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType,  identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readListBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if(elemType != " << valueThriftType << ") throw ThriftException(ThriftException::Type::INVALID_DATA, \"Incorrect list type ("
            << identPrefix + field.name <<")\");" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    " << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)  << " elem;" << endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.append(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "r.readListEnd();" << endl;
    }
    else if (readMethod.contains("readSetBegin"))
    {
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::SetType>()->valueType;
        QString valueReadMethod = typeToStr(valueType,  identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType,  identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readSetBegin(elemType, size);" << endl;
        out << indent << "v.reserve(size);" << endl;
        out << indent << "if(elemType != " << valueThriftType << ") throw ThriftException(ThriftException::Type::INVALID_DATA, \"Incorrect set type ("
            << identPrefix + field.name <<")\");" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    " << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)  << " elem;"<< endl;
        out << indent << "    " << valueReadMethod << "elem);" << endl;
        out << indent << "    v.insert(elem);" << endl;
        out << indent << "}" << endl;
        out << indent << "r.readSetEnd();" << endl;
    }
    else if (readMethod.contains("readMapBegin"))
    {
        QSharedPointer<Parser::Type> keyType = field.type.dynamicCast<Parser::MapType>()->keyType;
        QString keyReadMethod = typeToStr(keyType, identPrefix + field.name, MethodType::ReadMethod);
        QString keyThriftType = typeToStr(keyType, identPrefix + field.name, MethodType::ThriftFieldType);
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::MapType>()->valueType;
        QString valueReadMethod = typeToStr(valueType, identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType, identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << "qint32 size;" << endl;
        out << indent << "ThriftFieldType::type keyType;" << endl;
        out << indent << "ThriftFieldType::type elemType;" << endl;
        out << indent << "r.readMapBegin(keyType, elemType, size);" << endl;
        out << indent << "if(keyType != " << keyThriftType << ") throw ThriftException(ThriftException::Type::INVALID_DATA, \"Incorrect map key type ("
            << identPrefix + field.name <<")\");" << endl;
        out << indent << "if(elemType != " << valueThriftType << ") throw ThriftException(ThriftException::Type::INVALID_DATA, \"Incorrect map value type ("
            << identPrefix + field.name <<")\");" << endl;
        out << indent << "for(qint32 i = 0; i < size; i++) {" << endl;
        out << indent << "    " << typeToStr(keyType, identPrefix + field.name, MethodType::ReadTypeName)  << " key;"<< endl;
        out << indent << "    " << keyReadMethod << "key);" << endl;
        out << indent << "    " << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)  << " value;"<< endl;
        out << indent << "    " << valueReadMethod << "value);" << endl;
        out << indent << "    v[key] = value;" << endl;
        out << indent << "}" << endl;
        out << indent << "r.readMapEnd();" << endl;
    }
    else
    {
        out << indent << readMethod << "v);" << endl;
    }

    out << indent << fieldParent << field.name << " = v;" << endl;
}

void Generator::generateTypes(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = "types.h";
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    const QString EDAMErrorCodeHeaderFileName = "EDAMErrorCode.h";
    const QStringList additionalPreIncludes = QStringList() << EDAMErrorCodeHeaderFileName;
    const QStringList additionalPostIncludes = QStringList() << "<QSharedPointer>" << "<QMetaType>";

    writeHeaderHeader(hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    QFile EDAMErrorCodeHeaderFile(QDir(headerOutPath).absoluteFilePath(EDAMErrorCodeHeaderFileName));
    if (!EDAMErrorCodeHeaderFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated header file for writing: %1").arg(EDAMErrorCodeHeaderFile.fileName()).toStdString());
    }

    QTextStream houtEDAMErrorCode(&EDAMErrorCodeHeaderFile);
    houtEDAMErrorCode.setCodec("UTF-8");

    writeHeaderHeader(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    QList<Parser::Enumeration> enumerations = parser->enumerations();
    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it)
    {
        const Parser::Enumeration & e = *it;

        if (e.name == "EDAMErrorCode")
        {
            if (!e.docComment.isEmpty()) {
                houtEDAMErrorCode << e.docComment << endl;
            }

            houtEDAMErrorCode << "struct QEVERCLOUD_EXPORT " << e.name << endl << "{" << endl;
            houtEDAMErrorCode << "    enum type" << endl << "    {" << endl;

            for(int i = 0; i< e.values.length(); i++)
            {
                const QPair<QString, QString>& v = e.values[i];
                houtEDAMErrorCode << "        " << v.first;

                if (!v.second.isEmpty()) {
                    houtEDAMErrorCode << " = " << v.second;
                }

                if (i < (e.values.length() - 1)) {
                    houtEDAMErrorCode << ",";
                }

                houtEDAMErrorCode << endl;
            }

            houtEDAMErrorCode << "    };" << endl;
            houtEDAMErrorCode << "};" << endl << endl;
        }
        else
        {
            if (!e.docComment.isEmpty()) {
                hout << e.docComment << endl;
            }

            hout << "struct QEVERCLOUD_EXPORT " << e.name << " {" << endl;
            hout << "    enum type {" << endl;

            for(int i = 0; i< e.values.length(); i++)
            {
                const QPair<QString, QString> & v = e.values[i];
                hout << "        " << v.first;

                if (!v.second.isEmpty()) {
                    hout << " = " << v.second;
                }

                if (i < (e.values.length() - 1)) {
                    hout << ",";
                }

                hout << endl;
            }

            hout << "    };" << endl;
            hout << "};" << endl << endl;
        }
    }

    hout << endl;
    houtEDAMErrorCode << endl;

    QList<Parser::TypeDefinition> typedefs = parser->typedefs();
    for(auto it = typedefs.constBegin(), end = typedefs.constEnd(); it != end; ++it)
    {
        const Parser::TypeDefinition & t = *it;

        if (!t.docComment.isEmpty()) {
            hout << t.docComment << endl;
        }

        hout << "typedef " << typeToStr(t.type, t.name) << " " << t.name << ";" << endl << endl;
    }
    hout << endl;

    QSet<QString> safe;
    QList<Parser::Structure> ordered;

    QSet<QString> exceptions;
    QList<Parser::Structure> parserExceptions = parser->exceptions();
    for(auto it = parserExceptions.constBegin(), end = parserExceptions.constEnd(); it != end; ++it) {
        exceptions.insert(it->name);
    }

    QList<Parser::Structure> heap = parser->structures();
    heap.append(parser->exceptions());

    int count = heap.count();
    while(!heap.isEmpty())
    {
        int i = 0;
        while(i < heap.count())
        {
            const Parser::Structure s = heap[i];
            bool safeStruct = true;
            for(const Parser::Field & f : s.fields)
            {
                QString typeName = getIdentifier(f.type);
                QString typeName2;
                if (typeName.isEmpty())
                {
                    if (f.type.dynamicCast<Parser::SetType>()) {
                        typeName = getIdentifier(f.type.dynamicCast<Parser::SetType>()->valueType);
                    }
                    else if (f.type.dynamicCast<Parser::ListType>()) {
                        typeName = getIdentifier(f.type.dynamicCast<Parser::ListType>()->valueType);
                    }
                    else if (f.type.dynamicCast<Parser::MapType>()) {
                        typeName = getIdentifier(f.type.dynamicCast<Parser::MapType>()->valueType);
                        typeName2 = getIdentifier(f.type.dynamicCast<Parser::MapType>()->keyType);
                    }
                }

                if (!typeName.isEmpty() && (allstructs_.contains(typeName) || allexceptions_.contains(typeName)) && !safe.contains(typeName)) {
                    safeStruct = false;
                    break;
                }

                if (!typeName2.isEmpty() && (allstructs_.contains(typeName2) || allexceptions_.contains(typeName2)) && !safe.contains(typeName2)) {
                    safeStruct = false;
                    break;
                }
            }

            if (safeStruct) {
                safe << s.name;
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

    for(auto it = ordered.constBegin(), end = ordered.constEnd(); it != end; ++it)
    {
        const Parser::Structure & s = *it;
        if (!s.docComment.isEmpty()) {
            hout << s.docComment << endl;
        }
        else {
            hout << "/** NO DOC COMMENT ID FOUND */" << endl;
        }

        if (exceptions.contains(s.name))
        {
            hout << "class QEVERCLOUD_EXPORT " << s.name << ": public EvernoteException"
                << endl << "{" << endl << "public:" << endl;

            for(const Parser::Field & f : s.fields) {
                hout << "    " << fieldToStr(f) << ";" << endl;
            }

            hout << endl;
            hout << "    " << s.name << "();" << endl;
            hout << "    virtual ~" << s.name << "() throw() Q_DECL_OVERRIDE;" << endl;

            if (!s.fields.isEmpty()) {
                hout << endl;
                hout << QStringLiteral("    ") << s.name << QStringLiteral("(const ") << s.name << QStringLiteral(" & other);") << endl;
            }

            hout << "    const char * what() const throw() Q_DECL_OVERRIDE;" << endl;
            hout << "    virtual QSharedPointer<EverCloudExceptionData> exceptionData() const Q_DECL_OVERRIDE;" << endl;
        }
        else
        {
            hout << "struct QEVERCLOUD_EXPORT " << s.name << " {" << endl;
            for(auto fit = s.fields.begin(), fend = s.fields.end(); fit != fend; ++fit)
            {
                const Parser::Field & f = *fit;

                if (s.fieldComments.contains(f.name))
                {
                    QStringList lines = s.fieldComments[f.name].split('\n');
                    for(auto lit = lines.constBegin(), lend = lines.constEnd(); lit != lend; ++lit) {
                        hout << "    " << *lit << endl;
                    }
                }
                else
                {
                    hout << "    " << "/** NOT DOCUMENTED */" << endl;
                }

                hout << "    " << fieldToStr(f) << ";" << endl;
            }
        }

        hout << endl;
        hout << QStringLiteral("    bool operator==(const %1 & other) const").arg(s.name) << endl;
        hout << "    {" << endl;

        bool first = true;
        for(const Parser::Field & f : s.fields)
        {
            if(first) {
                first = false;
                hout << "        " << "return ";
            }
            else {
                hout << "        " << "    && ";
            }

            if (f.required == Parser::Field::RequiredFlag::Optional) {
                hout << QStringLiteral("%1.isEqual(other.%1)").arg(f.name) << endl;
            }
            else {
                hout << QStringLiteral("(%1 == other.%1)").arg(f.name) << endl;
            }
        }

        hout << "        ;" << endl << "    }" << endl << endl;
        hout << QStringLiteral("    bool operator!=(const %1 & other) const").arg(s.name) << endl;
        hout << "    {" << endl;
        hout << QStringLiteral("        return !(*this == other);") << endl;
        hout << "    }" << endl << endl;

        hout << "};" << endl << endl;
    }
    hout << endl;

    writeHeaderFooter(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    hout << endl;
    hout << "} // namespace qevercloud" << endl<< endl;

    for(auto it = ordered.constBegin(), end = ordered.constEnd(); it != end; ++it) {
        hout << "Q_DECLARE_METATYPE(qevercloud::" << it->name << ")" << endl;
    }
    hout << endl;

    QString guard = QString("QEVERCLOUD_GENERATED_%1_H").arg(headerFileName.split('.')[0].toUpper());
    hout << "#endif // " << guard << endl;

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString typesImplHeaderFileName = "types_impl.h";
    QFile typesImplHeaderFile(QDir(sourceOutPath).absoluteFilePath(typesImplHeaderFileName));
    if (!typesImplHeaderFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated header file for writing: %1").arg(typesImplHeaderFile.fileName()).toStdString());
    }

    QTextStream hout2(&typesImplHeaderFile);
    hout2.setCodec("UTF-8");

    writeHeaderHeader(hout2, typesImplHeaderFileName, QStringList() << "<generated/types.h>" << "../impl.h");

    hout2 << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    QList<Parser::Structure> structuresAndExceptions = parser->structures();
    structuresAndExceptions << parser->exceptions();

    for(auto it = structuresAndExceptions.constBegin(), end = structuresAndExceptions.constEnd(); it != end; ++it) {
        const Parser::Structure & s = *it;
        hout2 << "void write" << s.name << "(ThriftBinaryBufferWriter & w, const " << s.name << " & s);" << endl;
        hout2 << "void read" << s.name << "(ThriftBinaryBufferReader & r, " << s.name << " & s);" << endl;
    }
    hout2 << endl;

    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it) {
        const Parser::Enumeration & e = *it;
        hout2 << "void readEnum" << e.name << "(ThriftBinaryBufferReader & r, " << e.name << "::type & e);" << endl;
    }
    hout2 << endl;
    hout2 << "/** @endcond */" << endl;

    writeHeaderFooter(hout2, typesImplHeaderFileName);

    const QString bodyFileName = "types.cpp";
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated source file for writing: %1").arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    writeHeaderBody(bout, headerFileName, QStringList() << "../impl.h" << "types_impl.h");

    bout << "/** @cond HIDDEN_SYMBOLS  */" << endl << endl;

    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it)
    {
        const Parser::Enumeration & e = *it;

        bout <<  "void readEnum" << e.name << "(ThriftBinaryBufferReader & r, " << e.name << "::type & e) {" << endl;
        bout << "    qint32 i;" << endl;
        bout << "    r.readI32(i);" << endl;
        bout << "    switch(i) {" << endl;

        for(const QPair<QString, QString> & v : e.values) {
            QString value = e.name + "::" + v.first;
            bout << "    case static_cast<int>(" << value << "): e = " << value << "; break;" << endl;
        }

        bout << "    default: throw ThriftException(ThriftException::Type::INVALID_DATA, \"Incorrect value for enum "
             << e.name << "\");" << endl;
        bout << "    }" << endl;
        bout << "}" << endl << endl;
    }

    for(auto it = structuresAndExceptions.constBegin(), end = structuresAndExceptions.constEnd(); it != end; ++it)
    {
        const Parser::Structure & s = *it;

        if (exceptions.contains(s.name))
        {
            bout << s.name << QStringLiteral("::") << s.name << QStringLiteral("() {}") << endl;
            bout << s.name << QStringLiteral("::~") << s.name << QStringLiteral("() throw() {}") << endl;

            if (!s.fields.isEmpty())
            {
                bout << s.name << QStringLiteral("::") << s.name << QStringLiteral("(const ") << s.name
                    << QStringLiteral("& other) : EvernoteException(other)") << endl;
                bout << "{" << endl;
                for(const Parser::Field & f : s.fields) {
                    bout << QStringLiteral("   ") << f.name << QStringLiteral(" = other.") << f.name << QStringLiteral(";") << endl;
                }
                bout << "}" << endl;
            }
        }

        bout << "void write" << s.name << "(ThriftBinaryBufferWriter & w, const " << s.name << " & s) {" << endl;
        bout << "    w.writeStructBegin(\"" << s.name  << "\");" << endl;
        writeThriftWriteFields(bout, s.fields, s.name, "s.");
        bout << "    w.writeFieldStop();" << endl;
        bout << "    w.writeStructEnd();" << endl;
        bout << "}" << endl << endl;

        bout << "void read" << s.name << "(ThriftBinaryBufferReader & r, " << s.name << " & s) {" << endl;
        bout << "    QString fname;" << endl;
        bout << "    ThriftFieldType::type fieldType;" << endl;
        bout << "    qint16 fieldId;" << endl;

        for(const Parser::Field & field : s.fields)
        {
            if (field.required != Parser::Field::RequiredFlag::Optional) {
                bout << "    bool " << field.name << "_isset = false;" << endl;
            }
        }

        bout << "    r.readStructBegin(fname);" << endl;
        bout << "    while(true)" << endl << "    {" << endl;
        bout << "        r.readFieldBegin(fname, fieldType, fieldId);" << endl;
        bout << "        if (fieldType == ThriftFieldType::T_STOP) break;" << endl;

        for(const Parser::Field & field : s.fields)
        {
            bool optional = (field.required == Parser::Field::RequiredFlag::Optional);
            bout << "        if (fieldId == " << field.id << ") {" << endl;
            bout << "            if (fieldType == "
                 << typeToStr(field.type, s.name + "." + field.name, MethodType::ThriftFieldType)
                 << ") {" << endl;

            if (!optional) {
                bout << "                " << field.name << "_isset = true;" << endl;
            }

            writeThriftReadField(bout, field, s.name + ".", "s.");
            bout << "            } else {" << endl;
            bout << "                r.skip(fieldType);" << endl;
            bout << "            }" << endl;
            bout << "        } else" << endl;
        }

        bout << "        {" << endl;
        bout << "            r.skip(fieldType);" << endl;
        bout << "        }" << endl;
        bout << "        r.readFieldEnd();" << endl;
        bout << "    }" << endl;
        bout << "    r.readStructEnd();" << endl;

        for(const Parser::Field & field : s.fields)
        {
            if (field.required != Parser::Field::RequiredFlag::Optional) {
                bout << "    if(!" << field.name
                     << "_isset) throw ThriftException(ThriftException::Type::INVALID_DATA, \""
                     << s.name << "." << field.name << " has no value\");"
                     << endl;
            }
        }
        bout << "}" << endl << endl;
    }
    bout << endl;

    bout << "/** @endcond */" << endl << endl;
    bout << endl;

    writeBodyFooter(bout);
}

void Generator::generateServices(Parser * parser, const QString & outPath)
{
    // Generate header

    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = "services.h";
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if(!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    QStringList additionalPreIncludes = QStringList() << "../AsyncResult.h"
                                                      << "constants.h"
                                                      << "types.h";
    QStringList additionalPostIncludes = QStringList() << "<QObject>";

    writeHeaderHeader(hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    QList<Parser::Service> services = parser->services();
    for(auto it = services.constBegin(), end = services.constEnd(); it != end; ++it)
    {
        const Parser::Service & s = *it;

        if (!s.extends.isEmpty()) {
            throw std::runtime_error("extending services are not supported");
        }

        if (!s.docComment.isEmpty()) {
            hout << s.docComment << endl;
        }

        hout << "class QEVERCLOUD_EXPORT " << s.name << ": public QObject" << endl << "{" << endl;
        hout << "    Q_OBJECT" << endl;
        hout << "    Q_DISABLE_COPY(" << s.name << ")"<< endl;
        hout << "public:" << endl;

        if (s.name == "UserStore") {
            hout << "    explicit UserStore(QString host, QString authenticationToken = QString(), QObject * parent = 0);" << endl << endl;

        }
        else {
            hout << "    explicit NoteStore(QString noteStoreUrl = QString(), QString authenticationToken = QString(), QObject * parent = 0);" << endl;
            hout << "    explicit NoteStore(QObject * parent);" << endl << endl;
            hout << "    void setNoteStoreUrl(QString noteStoreUrl) { m_url = noteStoreUrl; }" << endl;
            hout << "    QString noteStoreUrl() { return m_url; }" << endl << endl;
        }

        hout << "    void setAuthenticationToken(QString authenticationToken) { m_authenticationToken = authenticationToken; }" << endl;
        hout << "    QString authenticationToken() { return m_authenticationToken; }" << endl << endl;

        for(const Parser::Function & func: s.functions)
        {
            if (func.isOneway) {
                throw std::runtime_error("oneway functions are not supported");
            }

            if (!func.docComment.isEmpty())
            {
                QStringList lines = func.docComment.split('\n');
                for(auto lit = lines.constBegin(), lend = lines.constEnd(); lit != lend; ++lit) {
                    hout << "    " << *lit << endl;
                }
            }

            hout << "    " << typeToStr(func.type, func.name) << " " << func.name << "(";
            int lastId = func.params.last().id;
            bool tokenParamIsPresent = false;
            for(const Parser::Field & param: func.params)
            {
                if (param.name == "authenticationToken")
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                    if (param.initializer) {
                        hout << " = " << valueToStr(param.initializer, param.type, func.name + ", " + param.name);
                    }

                    if (param.id != lastId || tokenParamIsPresent) {
                        hout << ", ";
                    }
                }
            }

            if (tokenParamIsPresent) {
                hout << "QString authenticationToken = QString()";
            }
            hout << ");" << endl << endl;

            hout << "    /** Asynchronous version of @link " << func.name << " @endlink */" << endl;
            hout << "    AsyncResult * " << func.name << "Async(";
            tokenParamIsPresent = false;
            for(const Parser::Field & param: func.params)
            {
                if (param.name == "authenticationToken")
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                    if (param.initializer) {
                        hout << " = " << valueToStr(param.initializer, param.type, func.name + ", " + param.name);
                    }

                    if(param.id != lastId || tokenParamIsPresent) {
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
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::Notebook >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::Tag >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::SavedSearch >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::NoteVersionId >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::SharedNotebook >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::LinkedNotebook >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::BusinessInvitation >)";
    metatypeDeclarations << "Q_DECLARE_METATYPE(QList< qevercloud::UserProfile >)";

    writeHeaderFooter(hout, headerFileName, metatypeDeclarations);

    // Generate source

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString bodyFileName = "services.cpp";
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString("Can't open the generated source file for writing: %1").arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    writeHeaderBody(bout, headerFileName, QStringList() << "../impl.h" << "types_impl.h");

    for(auto it = services.constBegin(), end = services.constEnd(); it != end; ++it)
    {
        const Parser::Service & s = *it;

        for(const Parser::Function & func : s.functions)
        {
            QString prepareParamsName = s.name + "_" + func.name + "_prepareParams";
            QString readReplyName = s.name + "_" + func.name + "_readReply";
            int lastId = func.params.last().id;
            bool isVoidResult = !func.type.dynamicCast<Parser::VoidType>().isNull();

            bout << "QByteArray " << prepareParamsName << "(";
            for(const Parser::Field & param : func.params)
            {
                bout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                if (param.id != lastId) {
                    bout << ", ";
                }
            }

            bout << ")" << endl;
            bout << "{" << endl;
            bout << "    ThriftBinaryBufferWriter w;" << endl;
            bout << "    qint32 cseqid = 0;" << endl;
            bout << "    w.writeMessageBegin(\"" << func.name << "\", ThriftMessageType::T_CALL, cseqid);" << endl;
            bout << "    w.writeStructBegin(\"" << s.name << "_" << func.name << "_pargs" << "\");" << endl;
            writeThriftWriteFields(bout, func.params, func.name, "");
            bout << "    w.writeFieldStop();" << endl;
            bout << "    w.writeStructEnd();" << endl;
            bout << "    w.writeMessageEnd();" << endl;
            bout << "    return w.buffer();" << endl;
            bout << "}" << endl << endl;

            bout << (isVoidResult ? QStringLiteral("void") : typeToStr(func.type, func.name))
                 << " " << readReplyName << "(QByteArray reply)" << endl;
            bout << "{" << endl;

            if (!isVoidResult) {
                bout << "    bool resultIsSet = false;" << endl;
                bout << "    " << typeToStr(func.type, func.name) << " result = " << typeToStr(func.type, func.name) << "();" << endl;
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
            bout << "      throw ThriftException(ThriftException::Type::INVALID_MESSAGE_TYPE);" << endl;
            bout << "    }" << endl;
            bout << "    if (fname.compare(\"" << func.name << "\") != 0) {" << endl;
            bout << "      r.skip(ThriftFieldType::T_STRUCT);" << endl;
            bout << "      r.readMessageEnd();" << endl;
            bout << "      throw ThriftException(ThriftException::Type::WRONG_METHOD_NAME);" << endl;
            bout << "    }" << endl << endl;

            bout << "    ThriftFieldType::type fieldType;" << endl;
            bout << "    qint16 fieldId;" << endl;
            bout << "    r.readStructBegin(fname);" << endl;
            bout << "    while(true) {" << endl;
            bout << "        r.readFieldBegin(fname, fieldType, fieldId);" << endl;
            bout << "        if(fieldType == ThriftFieldType::T_STOP) break;" << endl;

            if (!isVoidResult)
            {
                Parser::Field result;
                result.id = 0;
                result.name = "result";
                result.required = Parser::Field::RequiredFlag::Required;
                result.type = func.type;
                bout << "        if(fieldId == 0) {" << endl;
                bout << "            if(fieldType == " << typeToStr(func.type, func.name, MethodType::ThriftFieldType) << ") {" << endl;
                bout << "                resultIsSet = true;" << endl;
                writeThriftReadField(bout, result, func.name + ".", "");
                bout << "            } else {" << endl;
                bout << "                r.skip(fieldType);" << endl;
                bout << "            }" << endl;
                bout << "        }" << endl;
            }

            bool firstThrow = isVoidResult;
            for(const Parser::Field & th: func.throws)
            {
                if (firstThrow) {
                    firstThrow = false;
                    bout << "        ";
                }
                else {
                    bout << "       else ";
                }

                bout << "if(fieldId == "  << th.id << ") {" << endl;
                QString exceptionType = typeToStr(th.type, func.name + ", " + th.name);
                bout << "            if(fieldType == ThriftFieldType::T_STRUCT) {" << endl;
                bout << "                " << exceptionType << " e;" << endl;
                bout << "                read" << exceptionType << "(r, e);" << endl;

                if (exceptionType == "EDAMSystemException") {
                    bout << "                throwEDAMSystemException(e);" << endl;
                }
                else {
                    bout << "                throw e;" << endl;
                }

                bout << "            } else {" << endl;
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
                bout << "    if(!resultIsSet) throw ThriftException(ThriftException::Type::MISSING_RESULT, QStringLiteral(\"" << func.name << ": missing result\"));" << endl;
                bout << "    return result;" << endl;
            }

            bout << "}" << endl << endl;

            QString asyncReadFunctionName = readReplyName + "Async";
            bout << "QVariant " << asyncReadFunctionName << "(QByteArray reply)" << endl;
            bout << "{" << endl;
            if (isVoidResult) {
                bout << "    " << readReplyName << "(reply);" << endl;
                bout << "    return QVariant();" << endl;
            }
            else {
                bout << "    return QVariant::fromValue(" << readReplyName << "(reply));" << endl;
            }
            bout << "}" << endl << endl;

            bout << typeToStr(func.type, func.name) << " " << s.name << "::" << func.name << "(";
            bool tokenParamIsPresent = false;
            for(const Parser::Field& param : func.params)
            {
                if (param.name == "authenticationToken") {
                    tokenParamIsPresent = true;
                }
                else {
                    bout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                    if(param.id != lastId || tokenParamIsPresent) bout << ", ";
                }
            }

            if (tokenParamIsPresent) {
                bout << "QString authenticationToken";
            }
            bout << ")" << endl;
            bout << "{" << endl;

            if (tokenParamIsPresent) {
                bout << "    if(authenticationToken.isEmpty()) authenticationToken = m_authenticationToken;" << endl;
            }

            bout << "    QByteArray params = " << prepareParamsName << "(";
            for(const Parser::Field & param : func.params) {
                bout << param.name;
                if(param.id != lastId) bout << ", ";
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


            bout << "AsyncResult* " << s.name << "::" << func.name << "Async(";
            tokenParamIsPresent = false;
            for(const Parser::Field & param : func.params)
            {
                if (param.name == "authenticationToken") {
                    tokenParamIsPresent = true;
                }
                else {
                    bout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                    if (param.id != lastId || tokenParamIsPresent) {
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
                bout << "    if(authenticationToken.isEmpty()) authenticationToken = m_authenticationToken;" << endl;
            }

            bout << "    QByteArray params = " << prepareParamsName << "(";
            for(const Parser::Field & param: func.params)
            {
                bout << param.name;
                if (param.id != lastId) {
                    bout << ", ";
                }
            }
            bout << ");" << endl;
            bout << "    return new AsyncResult(m_url, params, " << asyncReadFunctionName << ");" << endl;

            bout << "}" << endl << endl;
        }
    }

    writeBodyFooter(bout);
}

void Generator::generateSources(Parser * parser, QString outPath)
{
    if (parser->unions().count() > 0) {
        throw std::runtime_error("unions are not suported.");
    }

    baseTypes_ << "bool" << "byte" << "i16" << "i32" << "i64" << "double" << "string" << "binary";

    QList<Parser::Structure> structures = parser->structures();
    for(auto it = structures.constBegin(), end = structures.constEnd(); it != end; ++it) {
        allstructs_ << it->name;
    }

    QList<Parser::Structure> exceptions = parser->exceptions();
    for(auto it = exceptions.constBegin(), end = exceptions.constEnd(); it != end; ++it) {
        allexceptions_ << it->name;
    }

    QList<Parser::Enumeration> enumerations = parser->enumerations();
    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it) {
        allenums_ << it->name;
    }

    QList<Parser::Include> includes = parser->includes();
    for(auto it = includes.constBegin(), end = includes.constEnd(); it != end; ++it) {
        QString s = it->name;
        s.replace(QChar('\"'), QString(""));
        s.chop(QString("thrift").length());
        includeList_ << s;
    }

    QList<Parser::TypeDefinition> typedefs = parser->typedefs();
    for(auto it = typedefs.constBegin(), end = typedefs.constEnd(); it != end; ++it)
    {
        auto casted = it->type.dynamicCast<Parser::BaseType>();
        if (!casted.isNull()) {
            typedefMap_[it->name] = casted->basetype;
        }
    }

    generateConstants(parser, outPath);
    generateTypes(parser, outPath);
    generateServices(parser, outPath);
}

