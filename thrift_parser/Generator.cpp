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

    QSharedPointer<Parser::StringValue> stringvalue = value.dynamicCast<Parser::StringValue>();
    QSharedPointer<Parser::LiteralValue> literalvalue = value.dynamicCast<Parser::LiteralValue>();
    QSharedPointer<Parser::ListValue> listvalue = value.dynamicCast<Parser::ListValue>();
    QSharedPointer<Parser::MapValue> mapvalue = value.dynamicCast<Parser::MapValue>();

    QString result;
    if (stringvalue)
    {
        result = QStringLiteral("QStringLiteral(") + stringvalue->value + QStringLiteral(")");
    }
    else if (literalvalue)
    {
        result = literalvalue->value;
    }
    else if (listvalue)
    {
        if (!settype && !listtype) {
            throw std::runtime_error(QString::fromUtf8("List initializer for a unsupported type for (%1)").arg(identifier).toStdString());
        }

        result = typeToStr(type, identifier) + QStringLiteral("()");
        for(auto it = listvalue->values.constBegin(), end = listvalue->values.constEnd(); it != end; ++it) {
            const QSharedPointer<Parser::ConstValue> & v = *it;
            result += " << " + valueToStr(v, QSharedPointer<Parser::Type>(nullptr), identifier);
        }
    }
    else if (mapvalue) {
        throw std::runtime_error(QString::fromUtf8("map constants are not implemented (%1)").arg(identifier).toStdString());
    }

    if (result.isEmpty()) {
        throw std::runtime_error(QString::fromUtf8("Error! unrecognized constant value (%1)").arg(identifier).toStdString());
    }

    return result;
}

void Generator::generateConstants(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = QStringLiteral("constants.h");
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
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

    QStringList additionalPreIncludes;
    additionalPreIncludes << QStringLiteral("<qt4helpers.h>");
    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

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

        bout << "const " << typeToStr(c.type, c.name) << " "
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

        QString ident = QStringLiteral("");

        bool isOptional = (field.required == Parser::Field::RequiredFlag::Optional);
        if (isOptional) {
            ident = QStringLiteral("    ");
            out << QStringLiteral("    if(") << fieldPrefix + field.name << QStringLiteral(".isSet()) {") << endl;
        }

        out << ident << QStringLiteral("    w.writeFieldBegin(QStringLiteral(\"")
            << field.name << QStringLiteral("\"), ")
            << typeToStr(field.type, identPrefix + QStringLiteral(". ") + field.name, MethodType::ThriftFieldType)
            << QStringLiteral(", ") << field.id << QStringLiteral(");") << endl;

        QString fieldMoniker = fieldPrefix + field.name + (isOptional ? QStringLiteral(".ref()") : QStringLiteral(""));
        QString writeMethod = typeToStr(field.type, identPrefix + QStringLiteral(",") + field.name, MethodType::WriteMethod);

        if (writeMethod.contains(QStringLiteral("writeListBegin")))
        {
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::ListType>()->valueType;

            out << ident << QStringLiteral("    w.writeListBegin(")
                << typeToStr(valueType, identPrefix + QStringLiteral(",") + field.name, MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker << QStringLiteral(".length());") << endl;

            out << ident << QStringLiteral("    for(") << typeToStr(field.type, QString(), MethodType::TypeName)
                << QStringLiteral("::const_iterator it = ") << fieldMoniker << QStringLiteral(".constBegin(), end = ")
                << fieldMoniker << QStringLiteral(".constEnd(); it != end; ++it) {") << endl;

            QString writeMethod = typeToStr(valueType, identPrefix + "," + field.name, MethodType::WriteMethod);
            out << ident << QStringLiteral("        ") << writeMethod << QStringLiteral("*it")
                << (writeMethod.contains(QStringLiteral("static_cast<")) ? QStringLiteral(")") : QStringLiteral(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeListEnd();") << endl;
        }
        else if(writeMethod.contains(QStringLiteral("writeSetBegin")))
        {
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::SetType>()->valueType;

            out << ident << QStringLiteral("    w.writeSetBegin(")
                << typeToStr(valueType, identPrefix + QStringLiteral(",") + field.name, MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker << QStringLiteral(".count());") << endl;

            out << ident << QStringLiteral("    for(") << typeToStr(field.type, QString(), MethodType::TypeName)
                << QStringLiteral("::const_iterator it = ") << fieldMoniker << QStringLiteral(".constBegin(), end = ")
                << fieldMoniker << QStringLiteral(".constEnd(); it != end; ++it) {") << endl;

            QString writeMethod = typeToStr(valueType, identPrefix + QStringLiteral(",") + field.name, MethodType::WriteMethod);
            out << ident << QStringLiteral("        ") << writeMethod << QStringLiteral("*it")
                << (writeMethod.contains(QStringLiteral("static_cast<")) ? QStringLiteral(")") : QStringLiteral(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeSetEnd();") << endl;
        }
        else if(writeMethod.contains(QStringLiteral("writeMapBegin")))
        {
            QSharedPointer<Parser::Type> keyType = field.type.dynamicCast<Parser::MapType>()->keyType;
            QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::MapType>()->valueType;

            out << ident << QStringLiteral("    w.writeMapBegin(")
                << typeToStr(keyType, identPrefix + QStringLiteral(",") + field.name, MethodType::ThriftFieldType)
                << QStringLiteral(", ") << typeToStr(valueType, identPrefix + QStringLiteral(",") + field.name, MethodType::ThriftFieldType)
                << QStringLiteral(", ") << fieldMoniker << QStringLiteral(".size());") << endl;

            out << ident << QStringLiteral("    for(") << typeToStr(field.type, QString(), MethodType::TypeName)
                << QStringLiteral("::const_iterator it = ") << fieldMoniker << QStringLiteral(".constBegin(), end = ")
                << fieldMoniker << QStringLiteral(".constEnd(); it != end; ++it) {") << endl;

            QString keyWriteMethod = typeToStr(keyType, identPrefix + QStringLiteral(",") + field.name, MethodType::WriteMethod);
            QString valueWriteMethod = typeToStr(valueType, identPrefix + QStringLiteral(",") + field.name, MethodType::WriteMethod);
            out << ident << QStringLiteral("        ") << keyWriteMethod << QStringLiteral("it.key()")
                << (keyWriteMethod.contains(QStringLiteral("static_cast<")) ? QStringLiteral(")") : QStringLiteral(""))
                << QStringLiteral(");") << endl;
            out << ident << QStringLiteral("        ") << valueWriteMethod << QStringLiteral("it.value()")
                << (valueWriteMethod.contains(QStringLiteral("static_cast<")) ? QStringLiteral(")") : QStringLiteral(""))
                << QStringLiteral(");") << endl;

            out << ident << QStringLiteral("    }") << endl;
            out << ident << QStringLiteral("    w.writeMapEnd();") << endl;
        }
        else
        {
            out << ident << QStringLiteral("    ") << writeMethod << fieldMoniker
                << (writeMethod.contains(QStringLiteral("static_cast<")) ? QStringLiteral(")") : QStringLiteral(""))
                << QStringLiteral(");") << endl;
        }

        out << ident << QStringLiteral("    w.writeFieldEnd();") << endl;
        if (isOptional) {
            out << QStringLiteral("    }") << endl;
        }
    }
}

void Generator::writeThriftReadField(QTextStream & out, const Parser::Field & field, QString identPrefix, QString fieldParent)
{
    const char * indent = "                ";

    out << indent << typeToStr(field.type, identPrefix + field.name, MethodType::ReadTypeName)
        << QStringLiteral(" v;") << endl;

    QString readMethod = typeToStr(field.type, identPrefix + field.name, MethodType::ReadMethod);
    if (readMethod.contains(QStringLiteral("readListBegin")))
    {
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::ListType>()->valueType;
        QString valueReadMethod = typeToStr(valueType,  identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType,  identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;") << endl;
        out << indent << QStringLiteral("r.readListBegin(elemType, size);") << endl;
        out << indent << QStringLiteral("v.reserve(size);") << endl;
        out << indent << QStringLiteral("if(elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"Incorrect list type (")
            << identPrefix + field.name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {") << endl;
        out << indent << QStringLiteral("    ") << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)
            << QStringLiteral(" elem;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod << QStringLiteral("elem);") << endl;
        out << indent << QStringLiteral("    v.append(elem);") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readListEnd();") << endl;
    }
    else if (readMethod.contains("readSetBegin"))
    {
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::SetType>()->valueType;
        QString valueReadMethod = typeToStr(valueType,  identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType,  identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;") << endl;
        out << indent << QStringLiteral("r.readSetBegin(elemType, size);") << endl;
        out << indent << QStringLiteral("v.reserve(size);") << endl;
        out << indent << QStringLiteral("if (elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"Incorrect set type (")
            << identPrefix + field.name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {") << endl;
        out << indent << QStringLiteral("    ") << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)
            << QStringLiteral(" elem;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod << QStringLiteral("elem);") << endl;
        out << indent << QStringLiteral("    v.insert(elem);") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readSetEnd();") << endl;
    }
    else if (readMethod.contains(QStringLiteral("readMapBegin")))
    {
        QSharedPointer<Parser::Type> keyType = field.type.dynamicCast<Parser::MapType>()->keyType;
        QString keyReadMethod = typeToStr(keyType, identPrefix + field.name, MethodType::ReadMethod);
        QString keyThriftType = typeToStr(keyType, identPrefix + field.name, MethodType::ThriftFieldType);
        QSharedPointer<Parser::Type> valueType = field.type.dynamicCast<Parser::MapType>()->valueType;
        QString valueReadMethod = typeToStr(valueType, identPrefix + field.name, MethodType::ReadMethod);
        QString valueThriftType = typeToStr(valueType, identPrefix + field.name, MethodType::ThriftFieldType);
        out << indent << QStringLiteral("qint32 size;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type keyType;") << endl;
        out << indent << QStringLiteral("ThriftFieldType::type elemType;") << endl;
        out << indent << QStringLiteral("r.readMapBegin(keyType, elemType, size);") << endl;
        out << indent << QStringLiteral("if (keyType != ") << keyThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"Incorrect map key type (")
            << identPrefix + field.name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("if (elemType != ") << valueThriftType
            << QStringLiteral(") throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"Incorrect map value type (")
            << identPrefix + field.name << QStringLiteral(")\"));") << endl;
        out << indent << QStringLiteral("for(qint32 i = 0; i < size; i++) {") << endl;
        out << indent << QStringLiteral("    ")
            << typeToStr(keyType, identPrefix + field.name, MethodType::ReadTypeName)
            << QStringLiteral(" key;") << endl;
        out << indent << QStringLiteral("    ") << keyReadMethod << QStringLiteral("key);") << endl;
        out << indent << QStringLiteral("    ") << typeToStr(valueType, identPrefix + field.name, MethodType::ReadTypeName)
            << QStringLiteral(" value;") << endl;
        out << indent << QStringLiteral("    ") << valueReadMethod << QStringLiteral("value);") << endl;
        out << indent << QStringLiteral("    v[key] = value;") << endl;
        out << indent << QStringLiteral("}") << endl;
        out << indent << QStringLiteral("r.readMapEnd();") << endl;
    }
    else
    {
        out << indent << readMethod << QStringLiteral("v);") << endl;
    }

    out << indent << fieldParent << field.name << QStringLiteral(" = v;") << endl;
}

void Generator::generateTypes(Parser * parser, const QString & outPath)
{
    const QString headerOutPath = generatedHeaderOutputPath(outPath);
    const QString headerFileName = QStringLiteral("types.h");
    QFile headerFile(QDir(headerOutPath).absoluteFilePath(headerFileName));
    if (!headerFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    const QString EDAMErrorCodeHeaderFileName = QStringLiteral("EDAMErrorCode.h");
    QStringList additionalPreIncludes = QStringList() << EDAMErrorCodeHeaderFileName;
    QStringList additionalPostIncludes = QStringList() << QStringLiteral("<QSharedPointer>")
                                                       << QStringLiteral("<QMetaType>");

    writeHeaderHeader(hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    QFile EDAMErrorCodeHeaderFile(QDir(headerOutPath).absoluteFilePath(EDAMErrorCodeHeaderFileName));
    if (!EDAMErrorCodeHeaderFile.open(QIODevice::WriteOnly|QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated header file for writing: %1").arg(EDAMErrorCodeHeaderFile.fileName()).toStdString());
    }

    QTextStream houtEDAMErrorCode(&EDAMErrorCodeHeaderFile);
    houtEDAMErrorCode.setCodec("UTF-8");

    writeHeaderHeader(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    QList<Parser::Enumeration> enumerations = parser->enumerations();
    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it)
    {
        const Parser::Enumeration & e = *it;

        if (e.name == QStringLiteral("EDAMErrorCode"))
        {
            if (!e.docComment.isEmpty()) {
                houtEDAMErrorCode << e.docComment << endl;
            }

            houtEDAMErrorCode << QStringLiteral("struct QEVERCLOUD_EXPORT ") << e.name << endl << QStringLiteral("{") << endl;
            houtEDAMErrorCode << QStringLiteral("    enum type") << endl << QStringLiteral("    {") << endl;

            for(int i = 0; i< e.values.length(); i++)
            {
                const QPair<QString, QString>& v = e.values[i];
                houtEDAMErrorCode << QStringLiteral("        ") << v.first;

                if (!v.second.isEmpty()) {
                    houtEDAMErrorCode << QStringLiteral(" = ") << v.second;
                }

                if (i < (e.values.length() - 1)) {
                    houtEDAMErrorCode << QStringLiteral(",");
                }

                houtEDAMErrorCode << endl;
            }

            houtEDAMErrorCode << QStringLiteral("    };") << endl;
            houtEDAMErrorCode << QStringLiteral("};") << endl << endl;
        }
        else
        {
            if (!e.docComment.isEmpty()) {
                hout << e.docComment << endl;
            }

            hout << QStringLiteral("struct QEVERCLOUD_EXPORT ") << e.name << QStringLiteral(" {") << endl;
            hout << QStringLiteral("    enum type {") << endl;

            for(int i = 0; i< e.values.length(); i++)
            {
                const QPair<QString, QString> & v = e.values[i];
                hout << "        " << v.first;

                if (!v.second.isEmpty()) {
                    hout << QStringLiteral(" = ") << v.second;
                }

                if (i < (e.values.length() - 1)) {
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

    QList<Parser::TypeDefinition> typedefs = parser->typedefs();
    for(auto it = typedefs.constBegin(), end = typedefs.constEnd(); it != end; ++it)
    {
        const Parser::TypeDefinition & t = *it;

        if (!t.docComment.isEmpty()) {
            hout << t.docComment << endl;
        }

        hout << QStringLiteral("typedef ") << typeToStr(t.type, t.name)
             << QStringLiteral(" ") << t.name << QStringLiteral(";") << endl << endl;
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
            hout << QStringLiteral("/** NO DOC COMMENT ID FOUND */") << endl;
        }

        if (exceptions.contains(s.name))
        {
            hout << QStringLiteral("class QEVERCLOUD_EXPORT ") << s.name << QStringLiteral(": public EvernoteException")
                 << endl << QStringLiteral("{") << endl << QStringLiteral("public:") << endl;

            for(const Parser::Field & f : s.fields) {
                hout << QStringLiteral("    ") << fieldToStr(f) << QStringLiteral(";") << endl;
            }

            hout << endl;
            hout << QStringLiteral("    ") << s.name << QStringLiteral("();") << endl;
            hout << QStringLiteral("    virtual ~") << s.name << QStringLiteral("() throw() Q_DECL_OVERRIDE;") << endl;

            if (!s.fields.isEmpty()) {
                hout << endl;
                hout << QStringLiteral("    ") << s.name << QStringLiteral("(const ") << s.name << QStringLiteral(" & other);") << endl;
            }

            hout << QStringLiteral("    const char * what() const throw() Q_DECL_OVERRIDE;") << endl;
            hout << QStringLiteral("    virtual QSharedPointer<EverCloudExceptionData> exceptionData() const Q_DECL_OVERRIDE;") << endl;
        }
        else
        {
            hout << QStringLiteral("struct QEVERCLOUD_EXPORT ") << s.name << QStringLiteral(" {") << endl;
            for(auto fit = s.fields.begin(), fend = s.fields.end(); fit != fend; ++fit)
            {
                const Parser::Field & f = *fit;

                if (s.fieldComments.contains(f.name))
                {
                    QStringList lines = s.fieldComments[f.name].split(QStringLiteral("\n"));
                    for(auto lit = lines.constBegin(), lend = lines.constEnd(); lit != lend; ++lit) {
                        hout << QStringLiteral("    ") << *lit << endl;
                    }
                }
                else
                {
                    hout << QStringLiteral("    /** NOT DOCUMENTED */") << endl;
                }

                hout << QStringLiteral("    ") << fieldToStr(f) << QStringLiteral(";") << endl;
            }
        }

        hout << endl;
        hout << QString::fromUtf8("    bool operator==(const %1 & other) const").arg(s.name) << endl;
        hout << QStringLiteral("    {") << endl;

        bool first = true;
        for(const Parser::Field & f : s.fields)
        {
            if(first) {
                first = false;
                hout << QStringLiteral("        return ");
            }
            else {
                hout << QStringLiteral("            && ");
            }

            if (f.required == Parser::Field::RequiredFlag::Optional) {
                hout << QStringLiteral("%1.isEqual(other.%1)").arg(f.name) << endl;
            }
            else {
                hout << QStringLiteral("(%1 == other.%1)").arg(f.name) << endl;
            }
        }

        hout << QStringLiteral("        ;") << endl << QStringLiteral("    }") << endl << endl;
        hout << QStringLiteral("    bool operator!=(const %1 & other) const").arg(s.name) << endl;
        hout << QStringLiteral("    {") << endl;
        hout << QStringLiteral("        return !(*this == other);") << endl;
        hout << QStringLiteral("    }") << endl << endl;

        hout << QStringLiteral("};") << endl << endl;
    }
    hout << endl;

    writeHeaderFooter(houtEDAMErrorCode, EDAMErrorCodeHeaderFileName);

    hout << endl;
    hout << QStringLiteral("} // namespace qevercloud") << endl<< endl;

    for(auto it = ordered.constBegin(), end = ordered.constEnd(); it != end; ++it) {
        hout << QStringLiteral("Q_DECLARE_METATYPE(qevercloud::") << it->name << QStringLiteral(")") << endl;
    }
    hout << endl;

    QString guard = QString::fromUtf8("QEVERCLOUD_GENERATED_%1_H").arg(headerFileName.split(QStringLiteral("."))[0].toUpper());
    hout << QStringLiteral("#endif // ") << guard << endl;

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString typesImplHeaderFileName = QStringLiteral("types_impl.h");
    QFile typesImplHeaderFile(QDir(sourceOutPath).absoluteFilePath(typesImplHeaderFileName));
    if (!typesImplHeaderFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated header file for writing: %1").arg(typesImplHeaderFile.fileName()).toStdString());
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

    for(auto it = structuresAndExceptions.constBegin(), end = structuresAndExceptions.constEnd(); it != end; ++it) {
        const Parser::Structure & s = *it;
        hout2 << QStringLiteral("void write") << s.name << QStringLiteral("(ThriftBinaryBufferWriter & w, const ")
              << s.name << QStringLiteral(" & s);") << endl;
        hout2 << QStringLiteral("void read") << s.name << QStringLiteral("(ThriftBinaryBufferReader & r, ")
              << s.name << QStringLiteral(" & s);") << endl;
    }
    hout2 << endl;

    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it) {
        const Parser::Enumeration & e = *it;
        hout2 << QStringLiteral("void readEnum") << e.name << QStringLiteral("(ThriftBinaryBufferReader & r, ")
              << e.name << QStringLiteral("::type & e);") << endl;
    }
    hout2 << endl;
    hout2 << QStringLiteral("/** @endcond */") << endl;

    writeHeaderFooter(hout2, typesImplHeaderFileName);

    const QString bodyFileName = QStringLiteral("types.cpp");
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated source file for writing: %1").arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    additionalPreIncludes.clear();
    additionalPreIncludes << QStringLiteral("../impl.h")
                          << QStringLiteral("types_impl.h")
                          << QStringLiteral("<qt4helpers.h>");

    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

    bout << QStringLiteral("/** @cond HIDDEN_SYMBOLS  */") << endl << endl;

    for(auto it = enumerations.constBegin(), end = enumerations.constEnd(); it != end; ++it)
    {
        const Parser::Enumeration & e = *it;

        bout <<  QStringLiteral("void readEnum") << e.name << QStringLiteral("(ThriftBinaryBufferReader & r, ")
             << e.name << QStringLiteral("::type & e) {") << endl;

        bout << QStringLiteral("    qint32 i;") << endl;
        bout << QStringLiteral("    r.readI32(i);") << endl;
        bout << QStringLiteral("    switch(i) {") << endl;

        for(const QPair<QString, QString> & v : e.values) {
            QString value = e.name + "::" + v.first;
            bout << QStringLiteral("    case static_cast<int>(") << value << QStringLiteral("): e = ")
                 << value << QStringLiteral("; break;") << endl;
        }

        bout << QStringLiteral("    default: throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"Incorrect value for enum ")
             << e.name << QStringLiteral("\"));") << endl;
        bout << QStringLiteral("    }") << endl;
        bout << QStringLiteral("}") << endl << endl;
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
                bout << QStringLiteral("{") << endl;
                for(const Parser::Field & f : s.fields) {
                    bout << QStringLiteral("   ") << f.name << QStringLiteral(" = other.") << f.name << QStringLiteral(";") << endl;
                }
                bout << QStringLiteral("}") << endl;
            }
        }

        bout << QStringLiteral("void write") << s.name << QStringLiteral("(ThriftBinaryBufferWriter & w, const ")
             << s.name << QStringLiteral(" & s) {") << endl;

        bout << QStringLiteral("    w.writeStructBegin(QStringLiteral(\"") << s.name  << QStringLiteral("\"));") << endl;
        writeThriftWriteFields(bout, s.fields, s.name, QStringLiteral("s."));
        bout << QStringLiteral("    w.writeFieldStop();") << endl;
        bout << QStringLiteral("    w.writeStructEnd();") << endl;
        bout << QStringLiteral("}") << endl << endl;

        bout << QStringLiteral("void read") << s.name << QStringLiteral("(ThriftBinaryBufferReader & r, ")
             << s.name << QStringLiteral(" & s) {") << endl;
        bout << QStringLiteral("    QString fname;") << endl;
        bout << QStringLiteral("    ThriftFieldType::type fieldType;") << endl;
        bout << QStringLiteral("    qint16 fieldId;") << endl;

        for(const Parser::Field & field : s.fields)
        {
            if (field.required != Parser::Field::RequiredFlag::Optional) {
                bout << QStringLiteral("    bool ") << field.name << QStringLiteral("_isset = false;") << endl;
            }
        }

        bout << QStringLiteral("    r.readStructBegin(fname);") << endl;
        bout << QStringLiteral("    while(true)") << endl << QStringLiteral("    {") << endl;
        bout << QStringLiteral("        r.readFieldBegin(fname, fieldType, fieldId);") << endl;
        bout << QStringLiteral("        if (fieldType == ThriftFieldType::T_STOP) break;") << endl;

        for(const Parser::Field & field : s.fields)
        {
            bool isOptional = (field.required == Parser::Field::RequiredFlag::Optional);
            bout << QStringLiteral("        if (fieldId == ") << field.id << QStringLiteral(") {") << endl;
            bout << QStringLiteral("            if (fieldType == ")
                 << typeToStr(field.type, s.name + "." + field.name, MethodType::ThriftFieldType)
                 << QStringLiteral(") {") << endl;

            if (!isOptional) {
                bout << QStringLiteral("                ") << field.name << QStringLiteral("_isset = true;") << endl;
            }

            writeThriftReadField(bout, field, s.name + QStringLiteral("."), QStringLiteral("s."));
            bout << QStringLiteral("            } else {") << endl;
            bout << QStringLiteral("                r.skip(fieldType);") << endl;
            bout << QStringLiteral("            }") << endl;
            bout << QStringLiteral("        } else") << endl;
        }

        bout << QStringLiteral("        {") << endl;
        bout << QStringLiteral("            r.skip(fieldType);") << endl;
        bout << QStringLiteral("        }") << endl;
        bout << QStringLiteral("        r.readFieldEnd();") << endl;
        bout << QStringLiteral("    }") << endl;
        bout << QStringLiteral("    r.readStructEnd();") << endl;

        for(const Parser::Field & field : s.fields)
        {
            if (field.required != Parser::Field::RequiredFlag::Optional) {
                bout << QStringLiteral("    if(!") << field.name
                     << QStringLiteral("_isset) throw ThriftException(ThriftException::Type::INVALID_DATA, QStringLiteral(\"")
                     << s.name << QStringLiteral(".") << field.name << QStringLiteral(" has no value\"));")
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
        throw std::runtime_error(QString::fromUtf8("Can't open the generated header file for writing: %1").arg(headerFile.fileName()).toStdString());
    }

    QTextStream hout(&headerFile);
    hout.setCodec("UTF-8");

    QStringList additionalPreIncludes = QStringList() << QStringLiteral("../AsyncResult.h")
                                                      << QStringLiteral("constants.h")
                                                      << QStringLiteral("types.h");
    QStringList additionalPostIncludes = QStringList() << QStringLiteral("<QObject>");

    writeHeaderHeader(hout, headerFileName, additionalPreIncludes, additionalPostIncludes);

    QList<Parser::Service> services = parser->services();
    for(auto it = services.constBegin(), end = services.constEnd(); it != end; ++it)
    {
        const Parser::Service & s = *it;

        if (!s.extends.isEmpty()) {
            throw std::runtime_error("extending services is not supported");
        }

        if (!s.docComment.isEmpty()) {
            hout << s.docComment << endl;
        }

        hout << QStringLiteral("class QEVERCLOUD_EXPORT ") << s.name << QStringLiteral(": public QObject") << endl
             << QStringLiteral("{") << endl;
        hout << QStringLiteral("    Q_OBJECT") << endl;
        hout << QStringLiteral("    Q_DISABLE_COPY(") << s.name << QStringLiteral(")") << endl;
        hout << QStringLiteral("public:") << endl;

        if (s.name == QStringLiteral("UserStore")) {
            hout << QStringLiteral("    explicit UserStore(QString host, QString authenticationToken = QString(), QObject * parent = 0);")
                 << endl << endl;
        }
        else {
            hout << QStringLiteral("    explicit NoteStore(QString noteStoreUrl = QString(), QString authenticationToken = QString(), QObject * parent = 0);") << endl;
            hout << QStringLiteral("    explicit NoteStore(QObject * parent);") << endl << endl;
            hout << QStringLiteral("    void setNoteStoreUrl(QString noteStoreUrl) { m_url = noteStoreUrl; }") << endl;
            hout << QStringLiteral("    QString noteStoreUrl() { return m_url; }") << endl << endl;
        }

        hout << QStringLiteral("    void setAuthenticationToken(QString authenticationToken) { m_authenticationToken = authenticationToken; }") << endl;
        hout << QStringLiteral("    QString authenticationToken() { return m_authenticationToken; }") << endl << endl;

        for(const Parser::Function & func: s.functions)
        {
            if (func.isOneway) {
                throw std::runtime_error("oneway functions are not supported");
            }

            if (!func.docComment.isEmpty())
            {
                QStringList lines = func.docComment.split(QStringLiteral("\n"));
                for(auto lit = lines.constBegin(), lend = lines.constEnd(); lit != lend; ++lit) {
                    hout << QStringLiteral("    ") << *lit << endl;
                }
            }

            hout << QStringLiteral("    ") << typeToStr(func.type, func.name) << QStringLiteral(" ")
                 << func.name << QStringLiteral("(");
            int lastId = func.params.last().id;
            bool tokenParamIsPresent = false;
            for(const Parser::Field & param: func.params)
            {
                if (param.name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(param.type, func.name + QStringLiteral(", ") + param.name, MethodType::FuncParamType)
                         << QStringLiteral(" ") << param.name;
                    if (param.initializer) {
                        hout << QStringLiteral(" = ") << valueToStr(param.initializer, param.type, func.name + QStringLiteral(", ") + param.name);
                    }

                    if (param.id != lastId || tokenParamIsPresent) {
                        hout << QStringLiteral(", ");
                    }
                }
            }

            if (tokenParamIsPresent) {
                hout << QStringLiteral("QString authenticationToken = QString()");
            }
            hout << QStringLiteral(");") << endl << endl;

            hout << QStringLiteral("    /** Asynchronous version of @link ") << func.name << QStringLiteral(" @endlink */") << endl;
            hout << QStringLiteral("    AsyncResult * ") << func.name << QStringLiteral("Async(");
            tokenParamIsPresent = false;
            for(const Parser::Field & param: func.params)
            {
                if (param.name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    hout << typeToStr(param.type, func.name + QStringLiteral(", ") + param.name, MethodType::FuncParamType)
                         << QStringLiteral(" ") << param.name;
                    if (param.initializer) {
                        hout << QStringLiteral(" = ") << valueToStr(param.initializer, param.type, func.name + QStringLiteral(", ") + param.name);
                    }

                    if(param.id != lastId || tokenParamIsPresent) {
                        hout << QStringLiteral(", ");
                    }
                }
            }

            if (tokenParamIsPresent) {
                hout << QStringLiteral("QString authenticationToken = QString()");
            }
            hout << QStringLiteral(");") << endl << endl;
        }

        hout << QStringLiteral("private:") << endl;
        hout << QStringLiteral("    QString m_url;") << endl;
        hout << QStringLiteral("    QString m_authenticationToken;") << endl;
        hout << QStringLiteral("};") << endl << endl;
    }

    QStringList metatypeDeclarations;
    metatypeDeclarations.reserve(6);
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::Notebook >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::Tag >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::SavedSearch >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::NoteVersionId >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::SharedNotebook >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::LinkedNotebook >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::BusinessInvitation >)");
    metatypeDeclarations << QStringLiteral("Q_DECLARE_METATYPE(QList< qevercloud::UserProfile >)");

    writeHeaderFooter(hout, headerFileName, metatypeDeclarations);

    // Generate source

    const QString sourceOutPath = generatedSourceOutputPath(outPath);
    const QString bodyFileName = QStringLiteral("services.cpp");
    QFile bodyFile(QDir(sourceOutPath).absoluteFilePath(bodyFileName));
    if (!bodyFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        throw std::runtime_error(QString::fromUtf8("Can't open the generated source file for writing: %1").arg(bodyFile.fileName()).toStdString());
    }

    QTextStream bout(&bodyFile);
    bout.setCodec("UTF-8");

    additionalPreIncludes.clear();
    additionalPreIncludes << QStringLiteral("../impl.h")
                          << QStringLiteral("types_impl.h")
                          << QStringLiteral("<qt4helpers.h>");
    writeHeaderBody(bout, headerFileName, additionalPreIncludes);

    for(auto it = services.constBegin(), end = services.constEnd(); it != end; ++it)
    {
        const Parser::Service & s = *it;

        for(const Parser::Function & func : s.functions)
        {
            QString prepareParamsName = s.name + QStringLiteral("_") + func.name + QStringLiteral("_prepareParams");
            QString readReplyName = s.name + QStringLiteral("_") + func.name + QStringLiteral("_readReply");
            int lastId = func.params.last().id;
            bool isVoidResult = !func.type.dynamicCast<Parser::VoidType>().isNull();

            bout << QStringLiteral("QByteArray ") << prepareParamsName << QStringLiteral("(");
            for(const Parser::Field & param : func.params)
            {
                bout << typeToStr(param.type, func.name + QStringLiteral(", ") + param.name, MethodType::FuncParamType)
                     << QStringLiteral(" ") << param.name;
                if (param.id != lastId) {
                    bout << QStringLiteral(", ");
                }
            }

            bout << QStringLiteral(")") << endl;
            bout << QStringLiteral("{") << endl;
            bout << QStringLiteral("    ThriftBinaryBufferWriter w;") << endl;
            bout << QStringLiteral("    qint32 cseqid = 0;") << endl;
            bout << QStringLiteral("    w.writeMessageBegin(QStringLiteral(\"") << func.name
                 << QStringLiteral("\"), ThriftMessageType::T_CALL, cseqid);") << endl;
            bout << QStringLiteral("    w.writeStructBegin(QStringLiteral(\"") << s.name
                 << QStringLiteral("_") << func.name << QStringLiteral("_pargs\"));") << endl;
            writeThriftWriteFields(bout, func.params, func.name, QStringLiteral(""));
            bout << QStringLiteral("    w.writeFieldStop();") << endl;
            bout << QStringLiteral("    w.writeStructEnd();") << endl;
            bout << QStringLiteral("    w.writeMessageEnd();") << endl;
            bout << QStringLiteral("    return w.buffer();") << endl;
            bout << QStringLiteral("}") << endl << endl;

            bout << (isVoidResult ? QStringLiteral("void") : typeToStr(func.type, func.name))
                 << QStringLiteral(" ") << readReplyName << QStringLiteral("(QByteArray reply)") << endl;
            bout << QStringLiteral("{") << endl;

            if (!isVoidResult) {
                bout << QStringLiteral("    bool resultIsSet = false;") << endl;
                bout << QStringLiteral("    ") << typeToStr(func.type, func.name) << QStringLiteral(" result = ")
                     << typeToStr(func.type, func.name) << QStringLiteral("();") << endl;
            }

            bout << QStringLiteral("    ThriftBinaryBufferReader r(reply);") << endl;
            bout << QStringLiteral("    qint32 rseqid = 0;") << endl;
            bout << QStringLiteral("    QString fname;") << endl;
            bout << QStringLiteral("    ThriftMessageType::type mtype;") << endl;
            bout << QStringLiteral("    r.readMessageBegin(fname, mtype, rseqid);") << endl;
            bout << QStringLiteral("    if (mtype == ThriftMessageType::T_EXCEPTION) {") << endl;
            bout << QStringLiteral("      ThriftException e = readThriftException(r);") << endl;
            bout << QStringLiteral("      r.readMessageEnd();") << endl;
            bout << QStringLiteral("      throw e;") << endl;
            bout << QStringLiteral("    }") << endl;
            bout << QStringLiteral("    if (mtype != ThriftMessageType::T_REPLY) {") << endl;
            bout << QStringLiteral("      r.skip(ThriftFieldType::T_STRUCT);") << endl;
            bout << QStringLiteral("      r.readMessageEnd();") << endl;
            bout << QStringLiteral("      throw ThriftException(ThriftException::Type::INVALID_MESSAGE_TYPE);") << endl;
            bout << QStringLiteral("    }") << endl;
            bout << QStringLiteral("    if (fname.compare(QStringLiteral(\"") << func.name << QStringLiteral("\")) != 0) {") << endl;
            bout << QStringLiteral("      r.skip(ThriftFieldType::T_STRUCT);") << endl;
            bout << QStringLiteral("      r.readMessageEnd();") << endl;
            bout << QStringLiteral("      throw ThriftException(ThriftException::Type::WRONG_METHOD_NAME);") << endl;
            bout << QStringLiteral("    }") << endl << endl;

            bout << QStringLiteral("    ThriftFieldType::type fieldType;") << endl;
            bout << QStringLiteral("    qint16 fieldId;") << endl;
            bout << QStringLiteral("    r.readStructBegin(fname);") << endl;
            bout << QStringLiteral("    while(true) {") << endl;
            bout << QStringLiteral("        r.readFieldBegin(fname, fieldType, fieldId);") << endl;
            bout << QStringLiteral("        if(fieldType == ThriftFieldType::T_STOP) break;") << endl;

            if (!isVoidResult)
            {
                Parser::Field result;
                result.id = 0;
                result.name = QStringLiteral("result");
                result.required = Parser::Field::RequiredFlag::Required;
                result.type = func.type;
                bout << QStringLiteral("        if(fieldId == 0) {") << endl;
                bout << QStringLiteral("            if(fieldType == ") << typeToStr(func.type, func.name, MethodType::ThriftFieldType)
                     << QStringLiteral(") {") << endl;
                bout << QStringLiteral("                resultIsSet = true;") << endl;
                writeThriftReadField(bout, result, func.name + QStringLiteral("."), QStringLiteral(""));
                bout << QStringLiteral("            } else {") << endl;
                bout << QStringLiteral("                r.skip(fieldType);") << endl;
                bout << QStringLiteral("            }") << endl;
                bout << QStringLiteral("        }") << endl;
            }

            bool firstThrow = isVoidResult;
            for(const Parser::Field & th: func.throws)
            {
                if (firstThrow) {
                    firstThrow = false;
                    bout << QStringLiteral("        ");
                }
                else {
                    bout << QStringLiteral("       else ");
                }

                bout << QStringLiteral("if (fieldId == ")  << th.id << QStringLiteral(") {") << endl;
                QString exceptionType = typeToStr(th.type, func.name + QStringLiteral(", ") + th.name);
                bout << QStringLiteral("            if (fieldType == ThriftFieldType::T_STRUCT) {") << endl;
                bout << QStringLiteral("                ") << exceptionType << QStringLiteral(" e;") << endl;
                bout << QStringLiteral("                read") << exceptionType << QStringLiteral("(r, e);") << endl;

                if (exceptionType == QStringLiteral("EDAMSystemException")) {
                    bout << QStringLiteral("                throwEDAMSystemException(e);") << endl;
                }
                else {
                    bout << QStringLiteral("                throw e;") << endl;
                }

                bout << QStringLiteral("            }") << endl;
                bout << QStringLiteral("            else {") << endl;
                bout << QStringLiteral("                r.skip(fieldType);") << endl;
                bout << QStringLiteral("            }") << endl;
                bout << QStringLiteral("        }") << endl;
            }

            bout << QStringLiteral("        else {") << endl;
            bout << QStringLiteral("            r.skip(fieldType);") << endl;
            bout << QStringLiteral("        }") << endl;
            bout << QStringLiteral("        r.readFieldEnd();") << endl;
            bout << QStringLiteral("    }") << endl;
            bout << QStringLiteral("    r.readStructEnd();") << endl;

            bout << QStringLiteral("    r.readMessageEnd();") << endl;

            if (!isVoidResult) {
                bout << QStringLiteral("    if (!resultIsSet) {") << endl;
                bout << QStringLiteral("        throw ThriftException(ThriftException::Type::MISSING_RESULT, QStringLiteral(\"")
                     << func.name << QStringLiteral(": missing result\"));") << endl;
                bout << QStringLiteral("    }") << endl;
                bout << QStringLiteral("    return result;") << endl;
            }

            bout << QStringLiteral("}") << endl << endl;

            QString asyncReadFunctionName = readReplyName + QStringLiteral("Async");
            bout << QStringLiteral("QVariant ") << asyncReadFunctionName << QStringLiteral("(QByteArray reply)") << endl;
            bout << QStringLiteral("{") << endl;
            if (isVoidResult) {
                bout << QStringLiteral("    ") << readReplyName << QStringLiteral("(reply);") << endl;
                bout << QStringLiteral("    return QVariant();") << endl;
            }
            else {
                bout << QStringLiteral("    return QVariant::fromValue(") << readReplyName << QStringLiteral("(reply));") << endl;
            }
            bout << QStringLiteral("}") << endl << endl;

            bout << typeToStr(func.type, func.name) << " " << s.name << "::" << func.name << "(";
            bool tokenParamIsPresent = false;
            for(const Parser::Field& param : func.params)
            {
                if (param.name == QStringLiteral("authenticationToken")) {
                    tokenParamIsPresent = true;
                }
                else {
                    bout << typeToStr(param.type, func.name + ", " + param.name, MethodType::FuncParamType) << " " << param.name;
                    if(param.id != lastId || tokenParamIsPresent) bout << ", ";
                }
            }

            if (tokenParamIsPresent) {
                bout << QStringLiteral("QString authenticationToken");
            }
            bout << QStringLiteral(")") << endl;
            bout << QStringLiteral("{") << endl;

            if (tokenParamIsPresent) {
                bout << QStringLiteral("    if (authenticationToken.isEmpty()) {") << endl;
                bout << QStringLiteral("        authenticationToken = m_authenticationToken;") << endl;
                bout << QStringLiteral("    }") << endl;
            }

            bout << QStringLiteral("    QByteArray params = ") << prepareParamsName << QStringLiteral("(");
            for(const Parser::Field & param : func.params) {
                bout << param.name;
                if(param.id != lastId) bout << QStringLiteral(", ");
            }
            bout << QStringLiteral(");") << endl;

            bout << QStringLiteral("    QByteArray reply = askEvernote(m_url, params);") << endl;
            if (isVoidResult) {
                bout << QStringLiteral("    ") << readReplyName << QStringLiteral("(reply);") << endl;
            }
            else {
                bout << QStringLiteral("    return ") << readReplyName << QStringLiteral("(reply);") << endl;
            }

            bout << QStringLiteral("}") << endl << endl;


            bout << QStringLiteral("AsyncResult* ") << s.name << QStringLiteral("::") << func.name << QStringLiteral("Async(");
            tokenParamIsPresent = false;
            for(const Parser::Field & param : func.params)
            {
                if (param.name == QStringLiteral("authenticationToken"))
                {
                    tokenParamIsPresent = true;
                }
                else
                {
                    bout << typeToStr(param.type, func.name + QStringLiteral(", ") + param.name, MethodType::FuncParamType)
                         << QStringLiteral(" ") << param.name;

                    if (param.id != lastId || tokenParamIsPresent) {
                        bout << QStringLiteral(", ");
                    }
                }
            }

            if (tokenParamIsPresent) {
                bout << QStringLiteral("QString authenticationToken");
            }
            bout << QStringLiteral(")") << endl;
            bout << QStringLiteral("{") << endl;

            if (tokenParamIsPresent) {
                bout << QStringLiteral("    if (authenticationToken.isEmpty()) {") << endl;
                bout << QStringLiteral("        authenticationToken = m_authenticationToken;") << endl;
                bout << QStringLiteral("    }") << endl;
            }

            bout << QStringLiteral("    QByteArray params = ") << prepareParamsName << QStringLiteral("(");
            for(const Parser::Field & param: func.params)
            {
                bout << param.name;
                if (param.id != lastId) {
                    bout << QStringLiteral(", ");
                }
            }
            bout << QStringLiteral(");") << endl;
            bout << QStringLiteral("    return new AsyncResult(m_url, params, ") << asyncReadFunctionName << QStringLiteral(");") << endl;

            bout << QStringLiteral("}") << endl << endl;
        }
    }

    writeBodyFooter(bout);
}

void Generator::generateSources(Parser * parser, QString outPath)
{
    if (parser->unions().count() > 0) {
        throw std::runtime_error("unions are not suported.");
    }

    baseTypes_ << QStringLiteral("bool") << QStringLiteral("byte") << QStringLiteral("i16") << QStringLiteral("i32")
               << QStringLiteral("i64") << QStringLiteral("double") << QStringLiteral("string") << QStringLiteral("binary");

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
        s.replace(QStringLiteral("\""), QStringLiteral(""));
        s.chop(QStringLiteral("thrift").length());
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

