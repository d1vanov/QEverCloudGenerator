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

#ifndef QEVERCLOUD_GENERATOR_THRIFT_PARSER_LEXER_H
#define QEVERCLOUD_GENERATOR_THRIFT_PARSER_LEXER_H

#include <QObject>

class Lexer : public QObject
{
    Q_OBJECT
public:
    enum class TerminalSymbolType
    {
        Comment2 = -2,
        Comment = -1,
        NoState = 0,
        Identifier = 1,
        IntegerNumber = 2,
        FloatNumber = 3,
        FloatNumber2 = 4,
        String = 5,
        String2 = 6,
        Delimiter = 7,
        DocComment = 8
    };

    struct TerminalSymbol
    {
        TerminalSymbolType m_type;
        QString m_data;
        QString m_fileName;
        int m_lineNum;

        TerminalSymbol(const TerminalSymbolType type, const QString & data,
                       const QString & fileName, const int lineNum) :
            m_type(type),
            m_data(data),
            m_fileName(fileName),
            m_lineNum(lineNum)
        {}
    };

public:
    explicit Lexer(QObject * parent = nullptr);

    const QList<TerminalSymbol> & terminals()
    {
        return m_terminals;
    }

    void feedFile(QString fileName);

private:
    void lex(QString fileName, const QString & text);

    struct LexContext
    {
        LexContext(const QString & fileName, const QString & text) :
            m_fileName(fileName),
            m_state(TerminalSymbolType::NoState),
            m_pos(0),
            m_lineNum(1),
            m_savedLineNum(0),
            m_text(&text)
        {}

        QString m_fileName;
        TerminalSymbolType m_state;

        int m_pos;
        int m_lineNum;
        int m_savedLineNum;

        QStringRef m_text;
    };

    void lexChar(const QChar ch, LexContext & ctx, QString & data);

    bool isNextChar(const QChar testChar, const QStringRef & text, int pos);
    bool isNextNextChar(const QChar testChar, const QStringRef & text, int pos);

private:
    QList<TerminalSymbol> m_terminals;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_LEXER_H
