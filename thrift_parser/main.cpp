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
#include "Parser.h"
#include "Generator.h"

#include <QCoreApplication>
#include <QDir>
#include <QDebug>

#include <cstdlib>
#include <stdexcept>

int main(int argc, char *argv[])
{
    // Fixed seed for rand() calls
    std::srand(1575003691);

    QCoreApplication app(argc, argv);

    try
    {
        if (qApp->arguments().length() != 3) {
            throw std::runtime_error(
                "Incorrect arguments (thriftDir generatedDir)");
        }

        QString thriftDir = qApp->arguments().at(1);
        QString generatedDir = qApp->arguments().at(2);

        Lexer lexer{&app};
        QDir dir(thriftDir);

        QStringList thriftFilesMask;
        thriftFilesMask << QStringLiteral("*.thrift");

        QStringList thriftFiles = dir.entryList(
            thriftFilesMask, QDir::Files, QDir::Name);
        for(auto it = thriftFiles.constBegin(),
            end = thriftFiles.constEnd(); it != end; ++it)
        {
            QString thriftFileAbsolutePath = dir.absoluteFilePath(*it);
            lexer.feedFile(thriftFileAbsolutePath);
        }

        Parser parser{&app};
        for (const auto & term: qAsConst(lexer.terminals()))
        {
            parser.setFileName(term.m_fileName);
            parser.feed(term.m_type, term.m_data);
            if (parser.isError())
            {
                QString error = parser.errorMessage();
                error += QStringLiteral(" in file ");
                error += term.m_fileName;
                error += QStringLiteral(" at line ");
                error += QString::number(term.m_lineNum);
                error += QStringLiteral(": ");
                error += term.m_data;
                error += QStringLiteral("\ndetected token type: ");
                error += QString::number(static_cast<int>(term.m_type));
                throw std::runtime_error(error.toStdString());
            }
        }

        parser.complete();
        if (parser.isError()) {
            throw std::runtime_error(
                QString::fromUtf8("Parser error at completion: %1")
                .arg(parser.errorMessage()).toStdString());
        }

        Generator generator;
        generator.generateSources(parser, generatedDir);
    }
    catch(const std::exception & e)
    {
        qDebug() << "\nFAILURE!!!";
        qDebug() << e.what();
        return 1;
    }
    catch(...)
    {
        qDebug() << "\nFAILURE!!! Unknown exception\n";
        return 2;
    }

    return 0;
}
