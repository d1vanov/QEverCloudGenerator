#include "Parser.h"
#include <stdexcept>
#include <QMap>
#include <QtDebug>
#include <cstdlib>

void* ParseAlloc(void *(*mallocProc)(std::size_t));
void ParseFree(void *p, void (*freeProc)(void*));
void Parse(void *lemon,  int yymajor, Term* yyminor, Parser* parser);

Parser::Parser(QObject *parent) :
    QObject(parent)
{
    parser = ParseAlloc(std::malloc);
    isError_ = false;
}

Parser::~Parser()
{
    ParseFree(parser, std::free);
}

void Parser::feed(Lexer::TerminalSymbolType type, QString value)
{
    if(delims_.isEmpty()) {
        delims_["("] = TERM_PAREN_OPEN;
        delims_[")"] = TERM_PAREN_CLOSE;
        delims_[":"] = TERM_COLON;
        delims_["="] = TERM_EQ;
        delims_["{"] = TERM_CURLY_OPEN;
        delims_["}"] = TERM_CURLY_CLOSE;
        delims_[","] = TERM_LISTSEP;
        delims_[";"] = TERM_LISTSEP;
        delims_["<"] = TERM_LT;
        delims_[">"] = TERM_GT;
        delims_["["] = TERM_BRACKET_OPEN;
        delims_["]"] = TERM_BRACKET_CLOSE;

        reserved_["include"] = TERM_INCLUDE;
        reserved_["cpp_include"] = TERM_CPP_INCLUDE;
        reserved_["namespace"] = TERM_NAMESPACE;
        reserved_["const"] = TERM_CONST;
        reserved_["typedef"] = TERM_TYPEDEF;
        reserved_["enum"] = TERM_ENUM;
        reserved_["struct"] = TERM_STRUCT;
        reserved_["union"] = TERM_UNION;
        reserved_["exception"] = TERM_EXCEPTION;
        reserved_["service"] = TERM_SERVICE;
        reserved_["extends"] = TERM_EXTENDS;
        reserved_["required"] = TERM_REQUIRED;
        reserved_["optional"] = TERM_OPTIONAL;
        reserved_["oneway"] = TERM_ONEWAY;
        reserved_["void"] = TERM_VOID;
        reserved_["throws"] = TERM_THROWS;
        reserved_["bool"] = TERM_BOOL;
        reserved_["byte"] = TERM_BYTE;
        reserved_["i16"] = TERM_I16;
        reserved_["i32"] = TERM_I32;
        reserved_["i64"] = TERM_I64;
        reserved_["double"] = TERM_DOUBLE;
        reserved_["string"] = TERM_STRING;
        reserved_["binary"] = TERM_BINARY;
        reserved_["map"] = TERM_MAP;
        reserved_["set"] = TERM_SET;
        reserved_["list"] = TERM_LIST;
    }

    int yymajor = 0;
    bool sendValue = false;
    switch(type) {
    case Lexer::TerminalSymbolType::DocComment:
        yymajor = TERM_DOC_COMMENT;
        sendValue = true;
        break;
    case Lexer::TerminalSymbolType::Identifier:
        if(reserved_.contains(value)) {
            yymajor = reserved_.value(value);
        } else {
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
        if(delims_.contains(value)) {
            yymajor = delims_.value(value);
        } else {
            throw std::runtime_error("Incorrect delimeter from lexer");
        }
        break;
    default: throw std::runtime_error("Incorrect lexer terminal type");
    }

    QString* lemonValue = nullptr;
    if(sendValue) {
        lemonValue = new QString(value);
    }
    Parse(parser, yymajor, lemonValue, this);
}

void Parser::complete()
{
    Parse(parser, TERM_END_OF_FILE, nullptr, this);
}

void Parser::addTypedef(QString name, QSharedPointer<Type> type, QString docComment)
{
    TypeDefinition td;
    td.file = file_;
    td.name = name;
    td.type = type;
    td.docComment = docComment;
    typedefs_.append(td);
}

void Parser::addNamespace(QString scope, QString name)
{
    Namespace n;
    n.file = file_;
    n.name = name;
    n.scope = scope;
    namespaces_.append(n);
}

void Parser::addConst(QSharedPointer<Type> type, QString name, QSharedPointer<ConstValue> value, QString docComment)
{
    Constant c;
    c.file = file_;
    c.name = name;
    c.type = type;
    c.value = value;
    c.docComment = docComment;
    constants_.append(c);
}

void Parser::addInclude(QString name)
{
    Include i;
    i.file = file_;
    i.name = name;
    includes_.append(i);
}

void Parser::addStructure(QString name, QList<Field> fields, QString docComment)
{
   Structure s;
   s.file = file_;
   s.name = name;
   s.fields = fields;
   s.parseStuctComment(docComment);
   structures_.append(s);
}

void Parser::addUnion(QString name, QList<Field> fields)
{
    Structure s;
    s.file = file_;
    s.name = name;
    s.fields = fields;
    unions_.append(s);
}

void Parser::addException(QString name, QList<Field> fields, QString docComment)
{
    Structure s;
    s.file = file_;
    s.name = name;
    s.fields = fields;
    s.docComment = docComment;
    exceptions_.append(s);
}

void Parser::addService(QString name, QString extends, QList<Parser::Function> functions, QString docComment)
{
    Service s;
    s.file = file_;
    s.name = name;
    s.extends = extends;
    s.functions = functions;
    s.docComment = docComment;
    services_.append(s);
}

void Parser::addEnumeration(QString name, QList<QPair<QString, QString> > values, QString docComment)
{
    Enumeration e;
    e.file = file_;
    e.name = name;
    e.values = values;
    e.docComment = docComment;
    enumerations_.append(e);
}



void Parser::Structure::parseStuctComment(QString rawComment)
{
    int pos = rawComment.indexOf("<dl>");
    if(pos < 0) {
        pos = rawComment.indexOf("<dt>");
    }
    if(pos < 0) {
        docComment = rawComment;
        return;
    }
    docComment = rawComment.left(pos) + "*/";
    rawComment = rawComment.mid(pos);
    rawComment.remove("*/").remove("<dl>").remove("</dl>");
    rawComment.replace("\n *", "\n");
    rawComment = rawComment.trimmed();

    while(!rawComment.isEmpty()) {
        int pos = rawComment.indexOf("<dt>");
        rawComment = rawComment.mid(pos + 4);
        pos = rawComment.indexOf("</dt>");
        if(pos < 0) {
            pos = rawComment.indexOf("\n");
        }
        QString fieldName = rawComment.left(pos).remove(':');
        QString fieldComment;
        rawComment = rawComment.mid(pos);
        rawComment.remove("</dt>");
        pos = rawComment.indexOf("</dd>");
        if(pos < 0) {
            pos = rawComment.indexOf("<dt>");
        }
        if(pos < 0) {
            fieldComment = rawComment.trimmed();
            rawComment = "";
        } else {
            fieldComment = rawComment.left(pos);
            rawComment = rawComment.mid(pos);
        }
        fieldComment = fieldComment.remove("</dd>").remove("<dd>").trimmed();
        this->fieldComments[fieldName] = "/**\n" + fieldComment + "\n*/";
    }

}
