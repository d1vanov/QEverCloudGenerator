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

class Generator
{
public:
    void generateSources(Parser * parser, const QString & outPath);

private:
    QString generatedHeaderOutputPath(const QString & outPath);
    QString generatedSourceOutputPath(const QString & outPath);

    void generateConstants(Parser * parser, const QString & outPath);
    void generateTypes(Parser * parser, const QString & outPath);
    void generateServices(Parser * parser, const QString & outPath);

    // Methods for writing header and source files

    void writeHeaderHeader(
        QTextStream & out, const QString & fileName,
        const QStringList & additionalPreIncludes = QStringList(),
        const QStringList & additionalPostIncludes = QStringList());

    void writeHeaderBody(
        QTextStream & out, const QString & headerFileName,
        const QStringList & additionalIncludes = QStringList());

    void writeHeaderFooter(
        QTextStream & out, const QString & fileName,
        const QStringList & extraContent = QStringList());

    void writeBodyFooter(QTextStream & out);

    void writeThriftReadField(
        QTextStream & out, const Parser::Field & field,
        const QString & identPrefix,
        const QString & fieldParent);

    void writeThriftWriteFields(
        QTextStream & out, const QList<Parser::Field> & fields,
        const QString & identPrefix, const QString & fieldPrefix);

    // Methods for taking a string representation of things

    QString valueToStr(
        QSharedPointer<Parser::ConstValue> value,
        QSharedPointer<Parser::Type> type,
        const QString & identifier);

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

private:
    QStringList m_includeList;
    QMap<QString, QString> m_typedefMap;
    QStringList m_baseTypes;
    QSet<QString> m_allStructs;
    QSet<QString> m_allExceptions;
    QSet<QString> m_allEnums;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_GENERATOR_H
