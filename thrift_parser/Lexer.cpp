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

#include "Lexer.h"

#include <QFile>
#include <QFileInfo>
#include <QTextStream>

#include <stdexcept>

Lexer::Lexer(QObject *parent) :
    QObject(parent)
{}

void Lexer::feedFile(QString fileName)
{
    QFile file(fileName);

    if (!file.open(QIODevice::ReadOnly|QIODevice::Text)) {
        throw std::runtime_error(
            QString::fromUtf8("Can't open file: %1")
            .arg(fileName).toStdString());
    }

    QTextStream in(&file);
    in.setCodec("UTF-8");
    lex(QFileInfo(fileName).fileName(), in.readAll());
}

void Lexer::lex(QString fileName, const QString & text)
{
    QString data = "";
    LexContext ctx(fileName, text);

    for(ctx.m_pos = 0; ctx.m_pos < text.length(); ++ctx.m_pos) {
        QChar ch = text[ctx.m_pos];
        if (ch == '\n') {
            ++ctx.m_lineNum;
        }

        lexChar(ch, ctx, data);
    }
}

void Lexer::lexChar(const QChar ch, LexContext & ctx, QString & data)
{
    switch(ctx.m_state)
    {
    case TerminalSymbolType::NoState:
        if (ch == ' ' || ch == '\t' || ch == '\n') {
            break;
        }
        else if (ch == '/' &&
                 isNextChar('*', ctx.m_text, ctx.m_pos) &&
                 isNextNextChar('*', ctx.m_text, ctx.m_pos))
        {
            ctx.m_state = TerminalSymbolType::DocComment;
            data.append(ch);
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '/' && isNextChar('*', ctx.m_text, ctx.m_pos)) {
            ctx.m_state = TerminalSymbolType::Comment;
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '/' && isNextChar('/', ctx.m_text, ctx.m_pos)) {
            ctx.m_state = TerminalSymbolType::Comment2;
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch.isLetter() || ch == '_') {
            ctx.m_state = TerminalSymbolType::Identifier;
            data.append(ch);
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '\"') {
            ctx.m_state = TerminalSymbolType::String;
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '\'') {
            ctx.m_state = TerminalSymbolType::String2;
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '(' || ch == ')' || ch == ':' || ch == '=' ||
                 ch == '{' || ch == '}' || ch == ',' || ch == '<' ||
                 ch == '>' || ch == '[' || ch == ']' || ch == ';')
        {
            m_terminals << TerminalSymbol(
                TerminalSymbolType::Delimiter, ch, ctx.m_fileName,
                ctx.m_lineNum);
            break;
        }
        else if (ch.isDigit() || ch == '+' || ch == '-') {
            data.append(ch);
            ctx.m_state = TerminalSymbolType::IntegerNumber;
            ctx.m_savedLineNum = ctx.m_lineNum;
            break;
        }
        else if (ch == '.') {
            data.append(ch);
            ctx.m_state = TerminalSymbolType::FloatNumber;
            break;
        }

        throw std::runtime_error(
            QString::fromUtf8("Syntax error at line %1 of the file \"%2\" : %3")
            .arg(ctx.m_lineNum).arg(ctx.m_fileName).arg(ch).toStdString());

        break;
    case TerminalSymbolType::DocComment:
        data.append(ch);
        if (ch == '*' && isNextChar('/', ctx.m_text, ctx.m_pos)) {
            ++ctx.m_pos;
            data.append('/');
            m_terminals << TerminalSymbol(
                TerminalSymbolType::DocComment, data, ctx.m_fileName,
                ctx.m_savedLineNum);
            data = "";
            ctx.m_state = TerminalSymbolType::NoState;
        }
        break;
    case TerminalSymbolType::Comment:
        if (ch == '*' && isNextChar('/', ctx.m_text, ctx.m_pos)) {
            ctx.m_state = TerminalSymbolType::NoState;
            ++ctx.m_pos;
        }
        break;
    case TerminalSymbolType::Comment2:
        if (ch == '\n') {
            ctx.m_state = TerminalSymbolType::NoState;
        }
        break;
    case TerminalSymbolType::Identifier:
        if (!ch.isLetterOrNumber() && ch != '_' && ch != '.')
        {
            --ctx.m_pos;
            if (ch == '\n') {
                --ctx.m_lineNum;
            }

            m_terminals << TerminalSymbol(
                TerminalSymbolType::Identifier, data, ctx.m_fileName,
                ctx.m_savedLineNum);
            data = "";
            ctx.m_state = TerminalSymbolType::NoState;
            break;
        }
        data.append(ch);
        break;
    case TerminalSymbolType::String:
        if (ch == '\"')
        {
            m_terminals << TerminalSymbol(
                TerminalSymbolType::String, "\"" + data + "\"", ctx.m_fileName,
                ctx.m_savedLineNum);
            data = "";
            ctx.m_state = TerminalSymbolType::NoState;
            break;
        }
        data.append(ch);
        break;
    case TerminalSymbolType::String2:
        if (ch == '\'')
        {
            m_terminals << TerminalSymbol(
                TerminalSymbolType::String, "\"" + data + "\"",
                ctx.m_fileName, ctx.m_savedLineNum);
            data = "";
            ctx.m_state = TerminalSymbolType::NoState;
            break;
        }
        data.append(ch);
        break;
    case TerminalSymbolType::Delimiter:
        throw std::runtime_error(
            QString("Internal error at line %1 of the file \"%2\" : %3")
            .arg(ctx.m_lineNum).arg(ctx.m_fileName).arg(ch).toStdString());
        break;
    case TerminalSymbolType::IntegerNumber:
        if (!ch.isDigit())
        {
            if (ch == '.') {
                --ctx.m_pos;
                ctx.m_state = TerminalSymbolType::NoState;
            }
            else if (ch == 'e' || ch == 'E') {
                --ctx.m_pos;
                ctx.m_state = TerminalSymbolType::FloatNumber;
            }
            else
            {
                --ctx.m_pos;
                if (ch == '\n') {
                    --ctx.m_lineNum;
                }

                m_terminals << TerminalSymbol(
                    TerminalSymbolType::IntegerNumber, data, ctx.m_fileName,
                    ctx.m_savedLineNum);
                data = "";
                ctx.m_state = TerminalSymbolType::NoState;
            }
            break;
        }
        data.append(ch);
        break;
    case TerminalSymbolType::FloatNumber:
        if (!ch.isDigit())
        {
            if (ch == 'e' || ch == 'E')
            {
                data.append(ch);
                if (isNextChar('+', ctx.m_text, ctx.m_pos) ||
                    isNextChar('-', ctx.m_text, ctx.m_pos))
                {
                    ++ctx.m_pos;
                    data.append(ctx.m_text.at(ctx.m_pos));
                }
                ctx.m_state = TerminalSymbolType::FloatNumber2;
            }
            else
            {
                --ctx.m_pos;
                if (ch == '\n') {
                    --ctx.m_lineNum;
                }

                m_terminals << TerminalSymbol(
                    TerminalSymbolType::FloatNumber, data, ctx.m_fileName,
                    ctx.m_savedLineNum);
                data = "";
                ctx.m_state = TerminalSymbolType::NoState;
            }
            break;
        }
        data.append(ch);
        break;
    case TerminalSymbolType::FloatNumber2:
        if (!ch.isDigit())
        {
            --ctx.m_pos;
            if (ch == '\n') {
                --ctx.m_lineNum;
            }

            m_terminals << TerminalSymbol(
                TerminalSymbolType::FloatNumber, data, ctx.m_fileName,
                ctx.m_savedLineNum);
            data = "";
            ctx.m_state = TerminalSymbolType::NoState;
            break;
        }
        data.append(ch);
        break;
    }
}

bool Lexer::isNextChar(const QChar testChar, const QStringRef & text, int pos)
{
    if (text.length() <= (pos + 1)) {
        return false;
    }

    return text.at(pos + 1) == testChar;
}

bool Lexer::isNextNextChar(
    const QChar testChar, const QStringRef & text, int pos)
{
    if (text.length() <= (pos + 2)) {
        return false;
    }

    return text.at(pos + 2) == testChar;
}
