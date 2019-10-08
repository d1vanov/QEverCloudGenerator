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

#ifndef QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
#define QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H

#include "Parser.h"

#include <QFile>

////////////////////////////////////////////////////////////////////////////////

enum class OutputFileType
{
    Interface,
    Implementation
};

////////////////////////////////////////////////////////////////////////////////

class Generator
{
public:
    void generateSources(Parser * parser, const QString & outPath);

private:
    struct OutputFileContext
    {
        OutputFileContext(
            const QString & fileName,
            const QString & outPath,
            const OutputFileType type);

        QFile m_file;
        QTextStream m_out;
    };

    void generateConstantsHeader(Parser * parser, const QString & outPath);
    void generateConstantsCpp(Parser * parser, const QString & outPath);

    void generateErrorsHeader(Parser * parser, const QString & outPath);
    void generateErrorsCpp(Parser * parser, const QString & outPath);

    void generateTypesIOHeader(Parser * parser, const QString & outPath);

    void generateTypesHeader(Parser * parser, const QString & outPath);
    void generateTypesCpp(Parser * parser, const QString & outPath);

    void generateServicesHeader(Parser * parser, const QString & outPath);
    void generateServicesCpp(Parser * parser, const QString & outPath);

    enum class ServiceClassType
    {
        NonDurable,
        Durable
    };

    void generateServiceClassDeclaration(
        const Parser::Service & service,
        const ServiceClassType serviceClassType,
        OutputFileContext & ctx);

    void generateServiceClassDefinition(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateDurableServiceClassDefinition(
        const Parser::Service & service, OutputFileContext & ctx);

    // Methods for writing header and source files

    enum class HeaderKind
    {
        Public,
        Private
    };

    void writeHeaderHeader(
        QTextStream & out, const QString & fileName,
        const QStringList & additionalIncludes = QStringList(),
        const HeaderKind headerKind = HeaderKind::Public);

    void writeHeaderBody(
        QTextStream & out, const QString & headerFileName,
        const QStringList & additionalIncludes = QStringList());

    void writeHeaderFooter(
        QTextStream & out, const QString & fileName,
        const QStringList & extraLinesInsideNamespace = QStringList(),
        const QStringList & extraLinesOutsideNamespace = QStringList());

    void writeBodyFooter(QTextStream & out);

    void writeThriftWriteFields(
        QTextStream & out, const QList<Parser::Field> & fields,
        const QString & identPrefix, const QString & fieldPrefix);

    void writeThriftReadField(
        QTextStream & out, const Parser::Field & field,
        const QString & identPrefix,
        const QString & fieldParent);

    void sortIncludes(QStringList & includes) const;

    // Methods for taking a string representation of things

    QString valueToStr(
        QSharedPointer<Parser::ConstValue> value,
        QSharedPointer<Parser::Type> type,
        const QString & identifier,
        const QString & offset = QString());

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

    QString typeToStr(
        QSharedPointer<Parser::Type> type, const QString & identifier,
        const MethodType methodType = MethodType::TypeName);

    // Other auxiliary methods

    QString getIdentifier(const QSharedPointer<Parser::Type> & type);

    QString clearInclude(const QString & s) const;
    QString clearTypedef(const QString & s) const;

    // Write methods for particular parsed fields

    void writeEnumeration(
        QTextStream & out, const Parser::Enumeration & e) const;

    void writeEnumerationPrintDeclaration(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

    void writeEnumerationPrintDefinition(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

private:
    QStringList m_includeList;
    QMap<QString, QString> m_typedefMap;
    QStringList m_baseTypes;
    QSet<QString> m_allStructs;
    QSet<QString> m_allExceptions;
    QSet<QString> m_allEnums;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
