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
        TerminalSymbolType type;
        QString data;
        QString file;
        int line;

        TerminalSymbol(
                TerminalSymbolType type, QString data, QString file, int line) :
            type(type),
            data(data),
            file(file),
            line(line)
        {}
    };


    explicit Lexer(QObject * parent = nullptr);

    QList<TerminalSymbol> terminals()
    {
        return terminals_;
    }

    void feedFile(QString fileName);

private:

    void lex(QString fileName, const QString &text);
    bool isNextChar(QChar testChar, QString text, int pos);
    bool isNextNextChar(QChar testChar, QString text, int pos);

private:
    QList<TerminalSymbol> terminals_;
};

#endif // QEVERCLOUD_GENERATOR_THRIFT_PARSER_LEXER_H
