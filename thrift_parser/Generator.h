#ifndef QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
#define QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H

#include "Parser.h"

class Generator
{
public:
    void generateSources(Parser * parser, QString outPath);

private:
    QString generatedHeaderOutputPath(const QString & outPath);
    QString generatedSourceOutputPath(const QString & outPath);

    void generateConstants(Parser * parser, const QString & outPath);
    void generateTypes(Parser * parser, const QString & outPath);
    void generateServices(Parser * parser, const QString & outPath);

    // Methods for writing header and source files
    void writeHeaderHeader(QTextStream & out, QString fileName,
                           QStringList additionalPreIncludes = QStringList(),
                           QStringList additionalPostIncludes = QStringList());
    void writeHeaderBody(QTextStream& out, QString headerFileName,
                         QStringList moreIncludes = QStringList());
    void writeHeaderFooter(QTextStream & out, QString fileName,
                           QStringList extraContent = QStringList());

    void writeBodyFooter(QTextStream & out);

    void writeThriftReadField(QTextStream & out, const Parser::Field & field, QString identPrefix, QString fieldParent);
    void writeThriftWriteFields(QTextStream & out, const QList<Parser::Field> & fields, QString identPrefix, QString fieldPrefix);

    // Methods for taking a string representation of things
    QString valueToStr(QSharedPointer<Parser::ConstValue> value, QSharedPointer<Parser::Type> type, QString identifier);
    QString fieldToStr(const Parser::Field & field);

    enum class MethodType
    {
        TypeName = 0,
        WriteMethod,
        ReadMethod,
        ThriftFieldType,
        ReadTypeName,
        FuncParamType
    };

    QString typeToStr(QSharedPointer<Parser::Type> type, QString identifier, MethodType methodType = MethodType::TypeName);

    // Other auxiliary methods
    QString getIdentifier(const QSharedPointer<Parser::Type> & type);

    QString clearInclude(QString s);
    QString clearTypedef(QString s);

private:
    QStringList includeList_;
    QMap<QString, QString> typedefMap_;
    QStringList baseTypes_;
    QSet<QString> allstructs_;
    QSet<QString> allexceptions_;
    QSet<QString> allenums_;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
