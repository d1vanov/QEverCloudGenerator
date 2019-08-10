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

#ifndef QEVERCLOUD_GENERATOR_PARSER_H
#define QEVERCLOUD_GENERATOR_PARSER_H

#include "thrift_lemon.h"
#include "Lexer.h"

#include <QObject>
#include <QtDebug>
#include <QString>
#include <QStringList>
#include <QSharedPointer>

using Term = QString;

class Parser : public QObject
{
    Q_OBJECT
public:
    explicit Parser(QObject * parent = nullptr);

    virtual ~Parser();

    void feed(const Lexer::TerminalSymbolType type, const QString & value);

    void setFile(QString file)
    {
        m_fileName = file;
        if (!m_fileNames.contains(file)) {
            m_fileNames << file;
        }
    }

    void complete();

    struct Namespace
    {
        QString file;
        QString scope;
        QString name;
    };

    struct Include
    {
        QString file;
        QString name;
    };

    class Type
    {
    public:
        virtual ~Type() = 0;
    };

    class VoidType: public Type
    {
    public:
        ~VoidType() override {}
    };

    class BaseType: public Type
    {
    public:
        QString basetype;
        ~BaseType() override {}
    };

    class IdentifierType: public Type
    {
    public:
        QString identifier;
        ~IdentifierType() override {}
    };

    class MapType: public Type
    {
    public:
        QSharedPointer<Type> keyType;
        QSharedPointer<Type> valueType;
        ~MapType() override {}
    };

    class SetType: public Type
    {
    public:
        QSharedPointer<Type> valueType;
        ~SetType() override {}
    };

    class ListType: public Type
    {
    public:
        QSharedPointer<Type> valueType;
        ~ListType() override {}
    };

    struct TypeDefinition
    {
        QString file;
        QString name;
        QString docComment;
        QSharedPointer<Type> type;
    };

    class ConstValue
    {
    public:
        virtual ~ConstValue() = 0;
    };

    class LiteralValue: public ConstValue
    {
    public:
        QString value;
    };

    class IntegerValue: public LiteralValue
    {
    public:
        ~IntegerValue() override {}
    };

    class DoubleValue: public LiteralValue
    {
    public:
        ~DoubleValue() override {}
    };

    class StringValue: public LiteralValue
    {
    public:
        ~StringValue() override {}
    };

    class IdentifierValue: public LiteralValue
    {
    public:
        ~IdentifierValue() override {}
    };

    class ListValue: public ConstValue
    {
    public:
        QList<QSharedPointer<ConstValue>> values;
        ~ListValue() override {}
    };

    class MapValue: public ConstValue
    {
    public:
        QList<QPair<QSharedPointer<ConstValue>,QSharedPointer<ConstValue>>> values;
        ~MapValue() override {}
    };

    struct Field
    {
        int id;
        enum class RequiredFlag {Default, Optional, Required} required;
        QSharedPointer<Type> type;
        QString name;
        QSharedPointer<ConstValue> initializer;
    };

    struct Structure
    {
        QString file;
        QString name;
        QString docComment;
        QList<Field> fields;
        QMap<QString, QString> fieldComments;
        void parseStuctComment(QString rawComment);
    };

    struct Enumeration
    {
        QString file;
        QString name;
        QString docComment;
        QList<QPair<QString, QString>> values;
    };

    struct Function
    {
        bool isOneway;
        QSharedPointer<Type> type;
        QString name;
        QList<Field> params;
        QList<Field> throws;
        QString docComment;
    };

    struct Service
    {
        QString file;
        QString name;
        QString extends;
        QString docComment;
        QList<Function> functions;
    };

    struct Constant
    {
        QString file;
        QSharedPointer<Type> type;
        QString name;
        QSharedPointer<ConstValue> value;
        QString docComment;
    };

    QStringList files() {return m_fileNames;}
    QList<Namespace> namespaces() {return m_namespaces;}
    QList<Include> includes() {return m_includes;}
    QList<TypeDefinition> typedefs() {return m_typedefs;}
    QList<Structure> structures() {return m_structures;}
    QList<Enumeration> enumerations() {return m_enumerations;}
    QList<Structure> exceptions() {return m_exceptions;}
    QList<Service> services() {return m_services;}
    QList<Constant> constants() {return m_constants;}
    QList<Structure> unions() {return m_unions;}

    bool isError() {return m_isError;}
    QString errorMessage() {return m_errorMessage;}

    void setErrorFlag(QString errorMessage)
    {
        m_isError = true;
        m_errorMessage = errorMessage;
    }

    void addTypedef(
        QString name, QSharedPointer<Type> type, QString docComment);

    void addNamespace(QString scope, QString name);

    void addConst(
        QSharedPointer<Type> type, QString name,
        QSharedPointer<ConstValue> value, QString docComment);

    void addInclude(QString name);
    void addStructure(QString name, QList<Field> fields, QString docComment);
    void addUnion(QString name, QList<Field> fields);
    void addException(QString name, QList<Field> fields, QString docComment);

    void addService(
        QString name, QString extends, QList<Function> functions,
        QString docComment);

    void addEnumeration(
        QString name, QList<QPair<QString, QString>> values,
        QString docComment);

private:
    void * m_parser;

    bool m_isError;
    QString m_errorMessage;

    QString m_fileName;
    QStringList m_fileNames;

    QList<Namespace> m_namespaces;
    QList<Include> m_includes;
    QList<TypeDefinition> m_typedefs;
    QList<Structure> m_structures;
    QList<Enumeration> m_enumerations;
    QList<Structure> m_exceptions;
    QList<Service> m_services;
    QList<Constant> m_constants;
    QList<Structure> m_unions;

    QMap<QString, int> m_delims;
    QMap<QString, int> m_reserved;
};

inline Parser::Type::~Type() {}
inline Parser::ConstValue::~ConstValue() {}

#endif // QEVERCLOUD_GENERATOR_PARSER_H
