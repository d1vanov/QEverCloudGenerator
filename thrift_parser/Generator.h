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
    Implementation,
    Test
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

    void generateServerHeader(Parser * parser, const QString & outPath);
    void generateServerCpp(Parser * parser, const QString & outPath);

    void generateTestServerHeaders(Parser * parser, const QString & outPath);
    void generateTestServerCpps(Parser * parser, const QString & outPath);

    void generateTestRandomDataGeneratorsHeader(
        Parser * parser, const QString & outPath);

    void generateTestRandomDataGeneratorsCpp(
        Parser * parser, const QString & outPath);

    void generateLocalDataStructDeclaration(
        OutputFileContext & ctx);

    void generateLocalDataStructDefinition(
        OutputFileContext & ctx);

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

    void generateServerClassDeclaration(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateServerClassDefinition(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateServerHelperFunctions(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateTestServerHelperClassDefinition(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateTestServerAsyncValueFetcherClassDefinition(
        const Parser::Service & service, OutputFileContext & ctx);

    void generateTestServerPrepareRequestParams(
        const Parser::Function & func,
        const QList<Parser::Enumeration> & enumerations,
        OutputFileContext & ctx);

    void generateTestServerPrepareRequestResponse(
        const Parser::Function & func,
        const QList<Parser::Enumeration> & enumerations,
        OutputFileContext & ctx);

    void generateTestServerPrepareRequestExceptionResponse(
        const Parser & parser,
        const Parser::Field & exceptionField,
        OutputFileContext & ctx);

    void generateTestServerHelperLambda(
        const Parser::Service & service,
        const Parser::Function & func,
        const Parser & parser,
        OutputFileContext & ctx,
        const QString & exceptionToThrow = {});

    void generateTestServerSocketSetup(
        const Parser::Service & service,
        const Parser::Function & func,
        OutputFileContext & ctx);

    enum class ServiceCallKind
    {
        Sync,
        Async
    };

    void generateTestServerServiceCall(
        const Parser::Service & service,
        const Parser::Function & func,
        const ServiceCallKind callKind,
        OutputFileContext & ctx,
        const QString & exceptionTypeToCatch = {},
        const QString & exceptionNameToCompare = {});

    void generateGetRandomValueExpression(
        const Parser::Field & field,
        const QString & prefix,
        const Parser & parser,
        QTextStream & out,
        const QString & end = QStringLiteral(";\n"));

    void verifyTypeIsBaseOrIdentifier(
        const QSharedPointer<Parser::Type> & type) const;

    void generateGetRandomExceptionExpression(
        const Parser::Field & field,
        const Parser::Structure & e,
        const QString & prefix,
        const Parser & parser,
        QTextStream & out);

    void generateGetThriftExceptionExpression(
        QTextStream & out);

    QString getGenerateRandomValueFunction(const QString & typeName) const;

    // Methods for writing header and source files

    enum class HeaderKind
    {
        Public,
        Private,
        Test
    };

    void writeHeaderHeader(
        QTextStream & out, const QString & fileName,
        const QStringList & additionalIncludes = QStringList(),
        const HeaderKind headerKind = HeaderKind::Public);

    void writeHeaderBody(
        QTextStream & out, const QString & headerFileName,
        const QStringList & additionalIncludes = QStringList(),
        const HeaderKind headerKind = HeaderKind::Public);

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

    QString fieldDeclarationToStr(const Parser::Field & field);

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
        const MethodType methodType = MethodType::TypeName) const;

    // Other auxiliary methods

    QString getIdentifier(const QSharedPointer<Parser::Type> & type);

    QString clearInclude(const QString & s) const;
    QString clearTypedef(const QString & s) const;

    /**
     * @brief loggableFields - filters out fields like authentication token,
     * consumer key, consumer secret etc which should not be present in the log
     * @param fields - fields to be filtered for logging
     * @return the list of non-secret fields which can be put into a log entry
     */
    QList<Parser::Field> loggableFields(const QList<Parser::Field> & fields) const;

    QString camelCaseToSnakeCase(const QString & input) const;

    QString capitalize(const QString & input) const;

    QString decapitalize(const QString & input) const;

    // Write methods for particular parsed fields

    void writeEnumeration(
        QTextStream & out, const Parser::Enumeration & e) const;

    void writeEnumerationPrintDeclaration(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

    void writeEnumerationPrintDefinition(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

    void writeStructPrintDefinition(
        QTextStream & out, const Parser::Structure & s,
        const Parser & parser) const;

private:
    QStringList m_includeList;
    QMap<QString, QString> m_typedefMap;
    QStringList m_baseTypes;
    QSet<QString> m_allStructs;
    QSet<QString> m_allExceptions;
    QSet<QString> m_allEnums;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
