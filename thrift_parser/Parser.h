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

#include <memory>

using Term = QString;

class Parser : public QObject
{
    Q_OBJECT
public:
    struct Namespace
    {
        QString m_fileName;
        QString m_scope;
        QString m_name;
    };

    struct Include
    {
        QString m_fileName;
        QString m_name;
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
        QString m_baseType;

        ~BaseType() override {}
    };

    class IdentifierType: public Type
    {
    public:
        QString m_identifier;

        ~IdentifierType() override {}
    };

    class MapType: public Type
    {
    public:
        std::shared_ptr<Type> m_keyType;
        std::shared_ptr<Type> m_valueType;

        ~MapType() override {}
    };

    class SetType: public Type
    {
    public:
        std::shared_ptr<Type> m_valueType;

        ~SetType() override {}
    };

    class ListType: public Type
    {
    public:
        std::shared_ptr<Type> m_valueType;

        ~ListType() override {}
    };

    struct TypeDefinition
    {
        QString m_fileName;
        QString m_name;
        QString m_docComment;
        std::shared_ptr<Type> m_type;
    };

    class ConstValue
    {
    public:
        virtual ~ConstValue() = 0;
    };

    class LiteralValue: public ConstValue
    {
    public:
        QString m_value;
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
        QList<std::shared_ptr<ConstValue>> m_values;

        ~ListValue() override {}
    };

    class MapValue: public ConstValue
    {
    public:
        using ValuesList =
            QList<std::pair<std::shared_ptr<ConstValue>,std::shared_ptr<ConstValue>>>;

        ValuesList m_values;

        ~MapValue() override {}
    };

    struct Field
    {
        enum class RequiredFlag {
            Default,
            Optional,
            Required
        };

        int m_id;
        RequiredFlag m_required;

        std::shared_ptr<Type> m_type;
        QString m_name;
        std::shared_ptr<ConstValue> m_initializer;
    };

    struct Structure
    {
        QString m_fileName;
        QString m_name;
        QString m_docComment;
        QList<Field> m_fields;
        QMap<QString, QString> m_fieldComments;

        void parseStructComment(QString rawComment);
    };

    struct Enumeration
    {
        QString m_fileName;
        QString m_name;
        QString m_docComment;
        QList<std::pair<QString, QString>> m_values;
    };

    struct Function
    {
        bool m_isOneway;
        std::shared_ptr<Type> m_type;
        QString m_name;
        QList<Field> m_params;
        QList<Field> m_throws;
        QString m_docComment;
    };

    struct Service
    {
        QString m_fileName;
        QString m_name;
        QString m_extends;
        QString m_docComment;
        QList<Function> m_functions;
    };

    struct Constant
    {
        QString m_fileName;
        std::shared_ptr<Type> m_type;
        QString m_name;
        std::shared_ptr<ConstValue> m_value;
        QString m_docComment;
    };

public:
    explicit Parser(QObject * parent = nullptr);

    virtual ~Parser();

    void feed(const Lexer::TerminalSymbolType type, const QString & value);

    void setFileName(const QString & fileName)
    {
        m_fileName = fileName;
        if (!m_fileNames.contains(fileName)) {
            m_fileNames << fileName;
        }
    }

    void complete();

    const QStringList & files() const
    {
        return m_fileNames;
    }

    const QList<Namespace> & namespaces() const
    {
        return m_namespaces;
    }

    const QList<Include> & includes() const
    {
        return m_includes;
    }

    const QList<TypeDefinition> & typedefs() const
    {
        return m_typedefs;
    }

    const QList<Structure> & structures() const
    {
        return m_structures;
    }

    const QList<Enumeration> & enumerations() const
    {
        return m_enumerations;
    }

    const QList<Structure> & exceptions() const
    {
        return m_exceptions;
    }

    const QList<Service> & services() const
    {
        return m_services;
    }

    const QList<Constant> & constants() const
    {
        return m_constants;
    }

    const QList<Structure> & unions() const
    {
        return m_unions;
    }

    bool isError() const
    {
        return m_isError;
    }

    const QString & errorMessage() const
    {
        return m_errorMessage;
    }

    void setErrorFlag(QString errorMessage)
    {
        m_isError = true;
        m_errorMessage = errorMessage;
    }

    void addTypedef(
        QString name, std::shared_ptr<Type> type, QString docComment);

    void addNamespace(QString scope, QString name);

    void addConst(
        std::shared_ptr<Type> type, QString name,
        std::shared_ptr<ConstValue> value, QString docComment);

    void addInclude(QString name);
    void addStructure(QString name, QList<Field> fields, QString docComment);
    void addUnion(QString name, QList<Field> fields);
    void addException(QString name, QList<Field> fields, QString docComment);

    void addService(
        QString name, QString extends, QList<Function> functions,
        QString docComment);

    void addEnumeration(
        QString name, QList<std::pair<QString, QString>> values,
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
