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

#ifndef QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
#define QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H

#include "Parser.h"

#include <QFile>
#include <QHash>
#include <QSet>

#include <memory>
#include <optional>

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
    void generateSources(Parser & parser, const QString & outPath);

private:
    struct OutputFileContext
    {
        OutputFileContext(
            const QString & fileName,
            const QString & outPath,
            const OutputFileType type,
            const QString & section = {});

        QFile m_file;
        OutputFileType m_type;
        QTextStream m_out;
    };

    void generateConstantsHeader(Parser & parser, const QString & outPath);
    void generateConstantsCpp(Parser & parser, const QString & outPath);

    void generateErrorsHeader(Parser & parser, const QString & outPath);
    void generateErrorsCpp(Parser & parser, const QString & outPath);

    void generateTypesIOHeader(Parser & parser, const QString & outPath);

    void generateTypesHeader(Parser & parser, const QString & outPath);
    void generateTypesCpp(Parser & parser, const QString & outPath);

    void generateTypeAliasesHeader(
        const Parser::TypeAliases & typeAliases, const QString & outPath);

    void generateTypeHeader(
        const Parser::Structure & s, const QString & outPath,
        const QString & fileSection);

    void generateTypeCpp(
        const Parser::Structure & s, const QString & outPath,
        const QString & fileSection);

    void generateTypeImplHeader(
        const Parser::Structure & s, const Parser::Enumerations & enumerations,
        const QString & outPath, const QString & fileSection);

    void generateTypeImplCpp(
        const Parser::Structure & s, const QString & outPath,
        const QString & fileSection);

    void generateExceptionDataClassDeclaration(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionDataClassConstructorWithArgsDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionDataClassDestructorDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionDataClassThrowExceptionMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionClassWhatMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionClassExceptionDataMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateServicesHeader(Parser & parser, const QString & outPath);
    void generateServicesCpp(Parser & parser, const QString & outPath);

    void generateServerHeader(Parser & parser, const QString & outPath);
    void generateServerCpp(Parser & parser, const QString & outPath);

    void generateTestServerHeaders(Parser & parser, const QString & outPath);
    void generateTestServerCpps(Parser & parser, const QString & outPath);

    void generateTestRandomDataGeneratorsHeader(
        Parser & parser, const QString & outPath);

    void generateTestRandomDataGeneratorsCpp(
        Parser & parser, const QString & outPath);

    void generateTypeLocalDataAccessoryMethodDeclarations(
        const QString & className, OutputFileContext & ctx,
        QString indent = QString());

    void generateTypeLocalDataAccessoryMethodDefinitions(
        const QString & className, OutputFileContext & ctx);

    void generateClassAccessoryMethodsForFieldDeclarations(
        const Parser::Field & field, OutputFileContext & ctx,
        QString indent = QString());

    void generateClassAccessoryMethodsForFieldDefinitions(
        const Parser::Structure & s, const Parser::Field & field,
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

    void verifyTypeIsValueOrIdentifier(
        const std::shared_ptr<Parser::Type> & type) const;

    QString getGenerateRandomValueFunction(const QString & typeName) const;

    // Methods for writing header and source files

    enum class HeaderKind
    {
        Public,
        Private,
        Test
    };

    void writeHeaderHeader(
        OutputFileContext & ctx, const QString & fileName,
        const QStringList & additionalIncludes = QStringList(),
        const HeaderKind headerKind = HeaderKind::Public,
        const QString & section = {});

    void writeHeaderBody(
        OutputFileContext & ctx, const QString & headerFileName,
        const QStringList & additionalIncludes = QStringList(),
        const HeaderKind headerKind = HeaderKind::Public);

    void writeHeaderFooter(
        QTextStream & out, const QString & fileName,
        const QStringList & extraLinesInsideNamespace = QStringList(),
        const QStringList & extraLinesOutsideNamespace = QStringList());

    void writeThriftWriteFields(
        QTextStream & out, const QList<Parser::Field> & fields,
        const QString & indentPrefix, const QString & fieldPrefix);

    void writeThriftReadField(
        QTextStream & out, const Parser::Field & field,
        const QString & indentPrefix,
        const QString & fieldParent);

    void sortIncludes(QStringList & includes) const;

    // Methods for taking a string representation of things

    QString valueToStr(
        std::shared_ptr<Parser::ConstValue> value,
        std::shared_ptr<Parser::Type> type,
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
        std::shared_ptr<Parser::Type> type,
        const QString & identifier = QString(),
        const MethodType methodType = MethodType::TypeName) const;

    // Other auxiliary methods

    QString getIdentifier(const std::shared_ptr<Parser::Type> & type);

    QString clearInclude(const QString & s) const;

    std::optional<Parser::PrimitiveType::Type> aliasedPrimitiveType(
        const QString & primitiveTypeAlias) const;

    QString aliasedTypeName(const QString & s) const;

    /**
     * @brief additionalIncludesForFields examines the passed in structure and
     *        returns the list containing some stdlib's or Qt's headers which
     *        need to be included to satisfy the structure's requirements.
     *
     * @param s     Structure for which additional includes are computed
     */
    QStringList additionalIncludesForFields(const Parser::Structure & s) const;

    /**
     * @brief dependentTypeNames provides a list of singular types (i.e.
     *        non-list/map/set ones) which are contained within the members of
     *        the passed in structure, members of structure members and so on
     *        recursively.
     *
     * @param s     Structure which fields' dependent type names are computed
     */
    QStringList dependentTypeNames(const Parser::Structure & s) const;

    /**
     * @brief loggableFields filters out fields like authentication token,
     *        consumer key, consumer secret etc which should not be present in
     *        the log
     *
     * @param fields            Fields to be filtered for logging
     * @return                  The list of non-secret fields which can be put
     *                          into a log entry
     */
    QList<Parser::Field> loggableFields(const QList<Parser::Field> & fields) const;

    bool shouldGenerateLocalDataMethods(const Parser::Structure & s) const;

    QString camelCaseToSnakeCase(const QString & input) const;

    QString capitalize(const QString & input) const;

    QString decapitalize(const QString & input) const;

    QString fieldTypeToStr(const Parser::Field & field) const;

    bool isFieldOfPrimitiveType(
        const Parser::Field & field, const QString & fieldTypeName) const;

    // Write methods for particular parsed fields

    void writeTypeProperties(
        const Parser::Structure & s, OutputFileContext & ctx);

    void writeNamespaceBegin(OutputFileContext & ctx);

    void writeNamespaceEnd(QTextStream & out);

    void writeEnumeration(
        OutputFileContext & ctx, const Parser::Enumeration & e) const;

    void writeEnumerationPrintDeclaration(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

    void writeEnumerationPrintDefinition(
        QTextStream & out, const Parser::Enumeration & e,
        const char * printer) const;

    void writeTypeImplPrintDefinition(
        QTextStream & out, const Parser::Structure & s) const;

private:
    QStringList m_includeList;

    QHash<QString, Parser::PrimitiveType::Type> m_primitiveTypeAliases;
    QSet<QString> m_stringTypeAliases;
    QSet<QString> m_byteArrayTypeAliases;

    QStringList m_baseTypes;
    QSet<QString> m_allStructs;
    QSet<QString> m_allExceptions;
    QSet<QString> m_allEnums;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
