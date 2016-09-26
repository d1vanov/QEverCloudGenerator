#include <QCoreApplication>
#include "Lexer.h"
#include <QDir>
#include <QtDebug>
#include "Parser.h"
#include "Generator.h"

int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    try
    {
        if (qApp->arguments().length() != 3) {
            throw std::runtime_error("Incorrect arguments (thriftDir generatedDir)");
        }

        QString thriftDir = qApp->arguments().at(1);
        QString generatedDir = qApp->arguments().at(2);

        Lexer * lexer = new Lexer(&app);
        QDir dir(thriftDir);

        QStringList thriftFilesMask;
        thriftFilesMask << "*.thrift";

        QStringList thriftFiles = dir.entryList(thriftFilesMask, QDir::Files, QDir::Name);
        for(auto it = thriftFiles.constBegin(), end = thriftFiles.constEnd(); it != end; ++it) {
            QString thriftFileAbsolutePath = dir.absoluteFilePath(*it);
            lexer->feedFile(thriftFileAbsolutePath);
        }

        Parser * parser = new Parser(&app);

        QList<Lexer::TerminalSymbol> terminals = lexer->terminals();
        for(auto it = terminals.constBegin(), end = terminals.constEnd(); it != end; ++it)
        {
            const Lexer::TerminalSymbol & term = *it;

            parser->setFile(term.file);
            parser->feed(term.type, term.data);
            if (parser->isError())
            {
                QString error = parser->errorMessage();
                error += " in file ";
                error += term.file;
                error += " at line ";
                error += QString::number(term.line);
                error += ": ";
                error += term.data;
                error += "\ndetected token type: ";
                error += QString::number(static_cast<int>(term.type));
                throw std::runtime_error(error.toStdString());
            }
        }

        parser->complete();
        if (parser->isError()) {
            throw std::runtime_error(QString("Parser error at completion: %1").arg(parser->errorMessage()).toStdString());
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
