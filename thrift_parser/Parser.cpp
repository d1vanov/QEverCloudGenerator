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

#include "Parser.h"

#include <QMap>
#include <QtDebug>

#include <memory>
#include <stdexcept>
#include <cstdlib>

void * ParseAlloc(void *(*mallocProc)(std::size_t));

void ParseFree(void *p, void (*freeProc)(void*));

void Parse(void * lemon, int yymajor, Term * yyminor, Parser * m_parser);

Parser::Parser(QObject *parent) :
    QObject(parent)
{
    m_parser = ParseAlloc(std::malloc);
    m_isError = false;
}

Parser::~Parser()
{
    ParseFree(m_parser, std::free);
}

void Parser::feed(Lexer::TerminalSymbolType type, const QString & value)
{
    if (m_delims.isEmpty())
    {
        m_delims[QStringLiteral("(")] = TERM_PAREN_OPEN;
        m_delims[QStringLiteral(")")] = TERM_PAREN_CLOSE;
        m_delims[QStringLiteral(":")] = TERM_COLON;
        m_delims[QStringLiteral("=")] = TERM_EQ;
        m_delims[QStringLiteral("{")] = TERM_CURLY_OPEN;
        m_delims[QStringLiteral("}")] = TERM_CURLY_CLOSE;
        m_delims[QStringLiteral(",")] = TERM_LISTSEP;
        m_delims[QStringLiteral(";")] = TERM_LISTSEP;
        m_delims[QStringLiteral("<")] = TERM_LT;
        m_delims[QStringLiteral(">")] = TERM_GT;
        m_delims[QStringLiteral("[")] = TERM_BRACKET_OPEN;
        m_delims[QStringLiteral("]")] = TERM_BRACKET_CLOSE;

        m_reserved[QStringLiteral("include")] = TERM_INCLUDE;
        m_reserved[QStringLiteral("cpp_include")] = TERM_CPP_INCLUDE;
        m_reserved[QStringLiteral("namespace")] = TERM_NAMESPACE;
        m_reserved[QStringLiteral("const")] = TERM_CONST;
        m_reserved[QStringLiteral("typedef")] = TERM_TYPEDEF;
        m_reserved[QStringLiteral("enum")] = TERM_ENUM;
        m_reserved[QStringLiteral("struct")] = TERM_STRUCT;
        m_reserved[QStringLiteral("union")] = TERM_UNION;
        m_reserved[QStringLiteral("exception")] = TERM_EXCEPTION;
        m_reserved[QStringLiteral("service")] = TERM_SERVICE;
        m_reserved[QStringLiteral("extends")] = TERM_EXTENDS;
        m_reserved[QStringLiteral("required")] = TERM_REQUIRED;
        m_reserved[QStringLiteral("optional")] = TERM_OPTIONAL;
        m_reserved[QStringLiteral("oneway")] = TERM_ONEWAY;
        m_reserved[QStringLiteral("void")] = TERM_VOID;
        m_reserved[QStringLiteral("throws")] = TERM_THROWS;
        m_reserved[QStringLiteral("bool")] = TERM_BOOL;
        m_reserved[QStringLiteral("byte")] = TERM_BYTE;
        m_reserved[QStringLiteral("i16")] = TERM_I16;
        m_reserved[QStringLiteral("i32")] = TERM_I32;
        m_reserved[QStringLiteral("i64")] = TERM_I64;
        m_reserved[QStringLiteral("double")] = TERM_DOUBLE;
        m_reserved[QStringLiteral("string")] = TERM_STRING;
        m_reserved[QStringLiteral("binary")] = TERM_BINARY;
        m_reserved[QStringLiteral("map")] = TERM_MAP;
        m_reserved[QStringLiteral("set")] = TERM_SET;
        m_reserved[QStringLiteral("list")] = TERM_LIST;
    }

    int yymajor = 0;
    bool sendValue = false;
    switch(type)
    {
    case Lexer::TerminalSymbolType::DocComment:
        yymajor = TERM_DOC_COMMENT;
        sendValue = true;
        break;
    case Lexer::TerminalSymbolType::Identifier:
        if (m_reserved.contains(value)) {
            yymajor = m_reserved.value(value);
        }
        else {
            yymajor = TERM_IDENTIFIER;
            sendValue = true;
        }
        break;
    case Lexer::TerminalSymbolType::IntegerNumber:
        yymajor = TERM_INTEGER_VALUE;
        sendValue = true;
        break;
    case Lexer::TerminalSymbolType::FloatNumber:
        yymajor = TERM_DOUBLE_VALUE;
        sendValue = true;
        break;
    case Lexer::TerminalSymbolType::String:
        yymajor = TERM_STRING_VALUE;
        sendValue = true;
        break;
    case Lexer::TerminalSymbolType::Delimiter:
        if (m_delims.contains(value)) {
            yymajor = m_delims.value(value);
        }
        else {
            throw std::runtime_error("Incorrect delimeter from lexer");
        }
        break;
    default:
        throw std::runtime_error("Incorrect lexer terminal type");
    }

    QString* lemonValue = nullptr;
    if (sendValue) {
        lemonValue = new QString(value);
    }
    Parse(m_parser, yymajor, lemonValue, this);
}

void Parser::complete()
{
    Parse(m_parser, TERM_END_OF_FILE, nullptr, this);
}

void Parser::addTypedef(
    QString name, std::shared_ptr<Type> type, QString docComment)
{
    TypeDefinition td;
    td.m_fileName = m_fileName;
    td.m_name = name;
    td.m_type = type;
    td.m_docComment = docComment;
    m_typedefs.append(td);
}

void Parser::addNamespace(QString scope, QString name)
{
    Namespace n;
    n.m_fileName = m_fileName;
    n.m_name = name;
    n.m_scope = scope;
    m_namespaces.append(n);
}

void Parser::addConst(
    std::shared_ptr<Type> type, QString name, std::shared_ptr<ConstValue> value,
    QString docComment)
{
    Constant c;
    c.m_fileName = m_fileName;
    c.m_name = name;
    c.m_type = type;
    c.m_value = value;
    c.m_docComment = docComment;
    m_constants.append(c);
}

void Parser::addInclude(QString name)
{
    Include i;
    i.m_fileName = m_fileName;
    i.m_name = name;
    m_includes.append(i);
}

void Parser::addStructure(QString name, QList<Field> fields, QString docComment)
{
   Structure s;
   s.m_fileName = m_fileName;
   s.m_name = name;
   s.m_fields = fields;
   s.parseStructComment(docComment);
   m_structures.append(s);
}

void Parser::addUnion(QString name, QList<Field> fields)
{
    Structure s;
    s.m_fileName = m_fileName;
    s.m_name = name;
    s.m_fields = fields;
    m_unions.append(s);
}

void Parser::addException(QString name, QList<Field> fields, QString docComment)
{
    Structure s;
    s.m_fileName = m_fileName;
    s.m_name = name;
    s.m_fields = fields;
    s.m_docComment = docComment;
    m_exceptions.append(s);
}

void Parser::addService(
    QString name, QString extends, QList<Parser::Function> functions,
    QString docComment)
{
    Service s;
    s.m_fileName = m_fileName;
    s.m_name = name;
    s.m_extends = extends;
    s.m_functions = functions;
    s.m_docComment = docComment;
    m_services.append(s);
}

void Parser::addEnumeration(
    QString name, QList<std::pair<QString, QString>> values, QString docComment)
{
    Enumeration e;
    e.m_fileName = m_fileName;
    e.m_name = name;
    e.m_values = values;
    e.m_docComment = docComment;
    m_enumerations.append(e);
}

void Parser::Structure::parseStructComment(QString rawComment)
{
    int pos = rawComment.indexOf(QStringLiteral("<dl>"));
    if (pos < 0) {
        pos = rawComment.indexOf(QStringLiteral("<dt>"));
    }
    if (pos < 0) {
        m_docComment = rawComment;
        return;
    }
    m_docComment = rawComment.left(pos) + QStringLiteral("*/");
    rawComment = rawComment.mid(pos);
    rawComment.remove(QStringLiteral("*/")).remove(QStringLiteral("<dl>"))
        .remove(QStringLiteral("</dl>"));
    rawComment.replace(QStringLiteral("\n *"), QStringLiteral("\n"));
    rawComment = rawComment.trimmed();

    while(!rawComment.isEmpty())
    {
        int pos = rawComment.indexOf(QStringLiteral("<dt>"));
        rawComment = rawComment.mid(pos + 4);

        pos = rawComment.indexOf(QStringLiteral("</dt>"));
        if (pos < 0) {
            pos = rawComment.indexOf(QStringLiteral("\n"));
        }

        QString fieldName = rawComment.left(pos).remove(QChar::fromLatin1(':'));
        QString fieldComment;
        rawComment = rawComment.mid(pos);
        rawComment.remove(QStringLiteral("</dt>"));
        pos = rawComment.indexOf(QStringLiteral("</dd>"));
        if (pos < 0) {
            pos = rawComment.indexOf(QStringLiteral("<dt>"));
        }

        if (pos < 0) {
            fieldComment = rawComment.trimmed();
            rawComment = QLatin1Literal("");
        }
        else {
            fieldComment = rawComment.left(pos);
            rawComment = rawComment.mid(pos);
        }

        fieldComment = fieldComment.remove(QStringLiteral("</dd>"))
            .remove(QStringLiteral("<dd>")).trimmed();

        m_fieldComments[fieldName] =
            QStringLiteral("/**\n") + fieldComment + QStringLiteral("\n*/");
    }
}
