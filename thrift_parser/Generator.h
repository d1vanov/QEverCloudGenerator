/**
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Sergey Skoblikov, 2015-2021 Dmitry Ivanov
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
    void generateTypesIOCpp(Parser & parser, const QString & outPath);

    void generateAllExceptionsHeader(Parser & parser, const QString & outPath);
    void generateExceptionsFwdHeader(Parser & parser, const QString & outPath);

    void generateAllTypesHeader(Parser & parser, const QString & outPath);
    void generateTypesFwdHeader(Parser & parser, const QString & outPath);

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

    void generateSerializationJsonHeader(
        const Parser::Structure & s, const QString & outPath);

    void generateSerializationJsonCpp(
        const Parser::Structure & s, const QString & outPath);

    void generateSerializeToJsonMethod(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateDeserializeFromJsonMethod(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionClassWhatMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionClassRaiseMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateExceptionClassCloneMethodDefinition(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateAllTypeBuildersHeader(
        Parser & parser, const QString & outPath);

    void generateTypeBuildersFwdHeader(
        const Parser & parser, const QString & outPath);

    void generateTypeBuilderHeader(
        const Parser::Structure & s, const QString & outPath,
        const QString & fileSection);

    void generateTypeBuilderCpp(
        const Parser::Structure & s, const Parser::Enumerations & enumerations,
        const QString & outPath, const QString & fileSection);

    void generateAllExceptionBuildersHeader(
        Parser & parser, const QString & outPath);

    void generateExceptionBuildersFwdHeader(
        const Parser & parser, const QString & outPath);

    void generateTypeBuildersTestHeader(
        const Parser & parser, const QString & outPath);

    void generateTypeBuildersTestCpp(
        const Parser & parser, const QString & outPath);

    void generateTypeBuildersTestMethod(
        const Parser::Structure & s,
        const QList<Parser::Enumeration> & enumerations,
        OutputFileContext & ctx, bool isException);

    void generateMetaTypesHeader(
        const Parser & parser, const QString & outPath);

    void generateMetaTypesCpp(
        const Parser & parser, const QString & outPath);

    void generateServiceHeader(
        const Parser::Service & service, const QString & outPath);

    void generateServiceCpp(
        const Parser::Service & service, const QString & outPath);

    void generateAllServicesHeader(
        Parser & parser, const QString & outPath);

    void generateServicesFwdHeader(
        Parser & parser, const QString & outPath);

    void generateFwdHeader(const QString & outPath);

    void generateServerHeader(
        const Parser::Service & service, const QString & outPath);

    void generateServerCpp(
        const Parser::Service & service, const QString & outPath);

    void generateTestServerHeader(
        const Parser::Service & service, const QString & outPath);

    void generateTestServerCpp(
        const Parser::Service & service, const QString & outPath,
        Parser & parser);

    void generateTestRandomDataGeneratorsHeader(
        Parser & parser, const QString & outPath);

    void generateTestRandomDataGeneratorsCpp(
        Parser & parser, const QString & outPath);

    void generateTestClearLocalIdsHeader(
        Parser & parser, const QString & outPath);

    void generateTestClearLocalIdsCpp(
        Parser & parser, const QString & outPath);

    void generateTypeLocalDataAccessoryMethodDeclarations(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateTypeLocalDataAccessoryMethodDefinitions(
        const Parser::Structure & s, OutputFileContext & ctx);

    void generateClassAccessoryMethodsForFieldDeclarations(
        const Parser::Field & field, OutputFileContext & ctx);

    void generateClassAccessoryMethodsForAuxiliaryFields(
        const Parser::Structure & s, OutputFileContext & ctx);

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
        Parser & parser,
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

    [[nodiscard]] QString getGenerateRandomValueFunction(
        const QString & typeName) const;

    // Methods for writing header and source files

    enum class HeaderKind
    {
        Public,
        Private,
        Test
    };

    void writeHeaderHeader(
        OutputFileContext & ctx, const QString & fileName,
        const QStringList & additionalIncludes = {},
        const HeaderKind headerKind = HeaderKind::Public,
        const QString & section = {},
        const QStringList & forwardDeclarationsOutsideNamespace = {});

    void writeHeaderBody(
        OutputFileContext & ctx, const QString & headerFileName,
        const QStringList & additionalIncludes = {},
        const HeaderKind headerKind = HeaderKind::Public,
        const int depth = 0);

    void writeHeaderFooter(
        QTextStream & out, const QString & fileName,
        const QStringList & extraLinesInsideNamespace = {},
        const QStringList & extraLinesOutsideNamespace = {},
        const QString & section = {});

    void writeThriftWriteFields(
        QTextStream & out, const QList<Parser::Field> & fields,
        const QString & indentPrefix, const QString & fieldPrefix);

    void writeThriftReadField(
        QTextStream & out, const Parser::Field & field,
        const QString & indentPrefix,
        const QString & fieldParent);

    void sortIncludes(QStringList & includes) const;

    // Methods for taking a string representation of things

    [[nodiscard]] QString valueToStr(
        std::shared_ptr<Parser::ConstValue> value,
        std::shared_ptr<Parser::Type> type,
        const QString & identifier,
        const QString & offset = {});

    enum class MethodType
    {
        TypeName = 0,
        WriteMethod,
        ReadMethod,
        ThriftFieldType,
        ReadTypeName,
        FuncParamType
    };

    [[nodiscard]] QString typeToStr(
        std::shared_ptr<Parser::Type> type,
        const QString & identifier = QString{},
        const MethodType methodType = MethodType::TypeName) const;

    // Other auxiliary methods

    [[nodiscard]] QString getIncludeGuard(
        const QString & fileName, const QString & section) const;

    [[nodiscard]] QString getIdentifier(
        const std::shared_ptr<Parser::Type> & type);

    [[nodiscard]] QString clearInclude(const QString & s) const;

    [[nodiscard]] std::optional<Parser::PrimitiveType::Type> aliasedPrimitiveType(
        const QString & primitiveTypeAlias) const;

    [[nodiscard]] QString aliasedTypeName(const QString & s) const;

    /**
     * @brief additionalIncludesForFields examines the passed in structure and
     *        returns the list containing some stdlib's or Qt's headers which
     *        need to be included to satisfy the structure's requirements.
     *
     * @param s     Structure for which additional includes are computed
     */
    [[nodiscard]] QStringList additionalIncludesForFields(
        const Parser::Structure & s) const;

    /**
     * @brief dependentTypeNames provides a list of singular types (i.e.
     *        non-list/map/set ones) which are contained within the members of
     *        the passed in structure, members of structure members and so on
     *        recursively.
     *
     * @param s     Structure which fields' dependent type names are computed
     */
    [[nodiscard]] QStringList dependentTypeNames(
        const Parser::Structure & s) const;

    /**
     * @brief loggableFields filters out fields like authentication token,
     *        consumer key, consumer secret etc which should not be present in
     *        the log
     *
     * @param fields            Fields to be filtered for logging
     * @return                  The list of non-secret fields which can be put
     *                          into a log entry
     */
    [[nodiscard]] QList<Parser::Field> loggableFields(
        const QList<Parser::Field> & fields) const;

    [[nodiscard]] bool shouldGenerateLocalDataMethods(
        const Parser::Structure & s) const;

    [[nodiscard]] bool shouldGenerateLocalId(const Parser::Structure & s) const;

    [[nodiscard]] bool structContainsFieldsWithLocalId(
        const Parser::Structure & s, const Parser::Structures & structs) const;

    /**
     * @return                  The list of structs which either contain local
     *                          id themselves or have fields which contain
     *                          local id
     */
    [[nodiscard]] Parser::Structures collectStructsWithLocalId(
        Parser & parser) const;

    [[nodiscard]] std::optional<Parser::Structure> structForType(
        const std::shared_ptr<Parser::Type> & type,
        const Parser & parser) const;

    [[nodiscard]] QString camelCaseToSnakeCase(const QString & input) const;

    [[nodiscard]] QString capitalize(const QString & input) const;

    [[nodiscard]] QString decapitalize(const QString & input) const;

    [[nodiscard]] QString fieldTypeToStr(const Parser::Field & field) const;

    [[nodiscard]] bool isFieldOfPrimitiveType(
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
