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

#ifndef QEVERCLOUD_GENERATOR_THRIFT_PARSER_PARSER_HELPER_H
#define QEVERCLOUD_GENERATOR_THRIFT_PARSER_PARSER_HELPER_H

#include "Parser.h"

#include <QString>

////////////////////////////////////////////////////////////////////////////////

class AbstractNoterminal
{
public:
    virtual ~AbstractNoterminal() = 0;
};

inline AbstractNoterminal::~AbstractNoterminal() {}

////////////////////////////////////////////////////////////////////////////////

class Fieldtype: public AbstractNoterminal
{
public:
    virtual QSharedPointer<Parser::Type> type() = 0;
};

////////////////////////////////////////////////////////////////////////////////

class VoidType: public Fieldtype
{
public:
    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::VoidType> p(new Parser::VoidType);
        return p;
    }
};

////////////////////////////////////////////////////////////////////////////////

class IdentifierFieldType: public Fieldtype
{
public:
    ~IdentifierFieldType() override {}

    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::IdentifierType> p(new Parser::IdentifierType);
        p->m_identifier = m_identifier;
        return p;
    }

public:
    QString m_identifier;
};

////////////////////////////////////////////////////////////////////////////////

class DefinitionType: public Fieldtype
{};

////////////////////////////////////////////////////////////////////////////////

class BasenameDefinitiontype: public DefinitionType
{
public:
    ~BasenameDefinitiontype() override {}

    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::BaseType> p(new Parser::BaseType);
        p->m_baseType = m_baseType;
        return p;
    }

public:
    QString m_baseType;
};

////////////////////////////////////////////////////////////////////////////////

class ContainerType: public AbstractNoterminal
{
public:
    virtual QSharedPointer<Parser::Type> type() = 0;
};

////////////////////////////////////////////////////////////////////////////////

class ContainerTypeDefinitionType: public DefinitionType
{
public:
    ContainerTypeDefinitionType() :
        m_containerType(nullptr)
    {}

    ~ContainerTypeDefinitionType() override
    {
        delete m_containerType;
    }

    QSharedPointer<Parser::Type> type() override
    {
        if (m_containerType) {
            return m_containerType->type();
        }

        throw std::runtime_error("containertype == nullptr");
    }

public:
    ContainerType* m_containerType;
};

////////////////////////////////////////////////////////////////////////////////

class MapType: public ContainerType
{
public:
    ~MapType() override {}

    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::MapType> p(new Parser::MapType);
        p->m_keyType = m_keyType;
        p->m_valueType = m_valueType;
        return p;
    }

public:
    QSharedPointer<Parser::Type> m_keyType;
    QSharedPointer<Parser::Type> m_valueType;
};

////////////////////////////////////////////////////////////////////////////////

class ListType: public ContainerType
{
public:
    ~ListType() override {}

    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::ListType> p(new Parser::ListType);
        p->m_valueType = m_valueType;
        return p;
    }

public:
    QSharedPointer<Parser::Type> m_valueType;
};

////////////////////////////////////////////////////////////////////////////////

class SetType: public ContainerType
{
public:
    ~SetType() override {}

    QSharedPointer<Parser::Type> type() override
    {
        QSharedPointer<Parser::SetType> p(new Parser::SetType);
        p->m_valueType = m_valueType;
        return p;
    }

public:
    QSharedPointer<Parser::Type> m_valueType;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_PARSER_HELPER_H
