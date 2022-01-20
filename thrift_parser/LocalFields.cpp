/**
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2022 Dmitry Ivanov
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

#include "LocalFields.h"

namespace qevercloud_generator {

bool shouldGenerateLocalDataFields(const Parser::Structure & s)
{
    if ((s.m_name == QStringLiteral("User")) ||
        (s.m_name == QStringLiteral("SharedNotebook")) ||
        (s.m_name == QStringLiteral("SharedNote")))
    {
        return true;
    }

    for (const auto & f: s.m_fields)
    {
        const auto * identifierType =
            dynamic_cast<Parser::IdentifierType*>(f.m_type.get());

        if (!identifierType) {
            continue;
        }

        if (f.m_name == QStringLiteral("guid") &&
            ((identifierType->m_identifier ==
              QStringLiteral("Types.Guid")) ||
             (identifierType->m_identifier ==
              QStringLiteral("Guid"))))
        {
            return true;
        }
    }

    return false;
}

bool shouldGenerateLocalId(const Parser::Structure & s) noexcept
{
    if (s.m_name == QStringLiteral("LinkedNotebook") ||
        s.m_name == QStringLiteral("SharedNote") ||
        s.m_name == QStringLiteral("SharedNotebook") ||
        s.m_name == QStringLiteral("User"))
    {
        return false;
    }

    return true;
}

LocalField generateLocalDataField()
{
    Parser::Field localDataField;
    localDataField.m_id = -1;
    localDataField.m_required =
        Parser::Field::RequiredFlag::Required;
    localDataField.m_affiliation =
        Parser::Field::Affiliation::Local;

    auto hashType = std::make_shared<Parser::HashType>();
    hashType->m_keyType = std::make_shared<Parser::StringType>();
    hashType->m_valueType = std::make_shared<Parser::VariantType>();
    localDataField.m_type = std::move(hashType);

    localDataField.m_name = QStringLiteral("localData");

    return LocalField{
        std::move(localDataField),
        QStringLiteral(
            "/**\n"
            " @brief localData property can be used to store any "
            "additional\n"
            " data which might be needed to be set for the type object"
            "\n"
            " by QEverCloud's client code\n"
            "*/")};
}

LocalField generateLocallyFavoritedField()
{
    Parser::Field isLocallyFavoritedField;
    isLocallyFavoritedField.m_id = -1;
    isLocallyFavoritedField.m_required =
        Parser::Field::RequiredFlag::Required;
    isLocallyFavoritedField.m_affiliation =
        Parser::Field::Affiliation::Local;
    isLocallyFavoritedField.m_type =
        std::make_shared<Parser::PrimitiveType>(
            Parser::PrimitiveType::Type::Bool);
    isLocallyFavoritedField.m_name =
        QStringLiteral("isLocallyFavorited");

    auto initializer = std::make_shared<Parser::LiteralValue>();
    initializer->m_value = QStringLiteral("false");
    isLocallyFavoritedField.m_initializer = std::move(initializer);

    auto setterDefaultValue =
        std::make_shared<Parser::LiteralValue>();
    setterDefaultValue->m_value = QStringLiteral("true");
    isLocallyFavoritedField.m_setterDefaultValue =
        std::move(setterDefaultValue);
    isLocallyFavoritedField.m_overridePropertyName =
        QStringLiteral("favorited");

    return LocalField{
        std::move(isLocallyFavoritedField),
        QStringLiteral(
            "/**\n"
            " @brief locallyFavorited property can be used to keep "
            "track which\n"
            " data items were favorited in the client. "
            "Unfortunately,\n"
            " Evernote has never provided a way to synchronize "
            "such\n"
            " a property between different clients\n"
            "*/")};
}

LocalField generateLocalOnlyField()
{
    Parser::Field isLocalOnlyField;
    isLocalOnlyField.m_id = -1;
    isLocalOnlyField.m_required =
        Parser::Field::RequiredFlag::Required;
    isLocalOnlyField.m_affiliation =
        Parser::Field::Affiliation::Local;
    isLocalOnlyField.m_type =
        std::make_shared<Parser::PrimitiveType>(
            Parser::PrimitiveType::Type::Bool);
    isLocalOnlyField.m_name = QStringLiteral("isLocalOnly");

    auto initializer = std::make_shared<Parser::LiteralValue>();
    initializer->m_value = QStringLiteral("false");
    isLocalOnlyField.m_initializer = std::move(initializer);

    auto setterDefaultValue =
        std::make_shared<Parser::LiteralValue>();
    setterDefaultValue->m_value = QStringLiteral("true");
    isLocalOnlyField.m_setterDefaultValue =
        std::move(setterDefaultValue);

    return LocalField{
        std::move(isLocalOnlyField),
        QStringLiteral(
            "/**\n"
            " @brief localOnly flag can be used to keep track which\n"
            " data items are meant to be local only and thus never "
            "be synchronized\n"
            " with Evernote service\n"
            "*/")};
}

LocalField generateLocallyModifiedField()
{
    Parser::Field isLocallyModifiedField;
    isLocallyModifiedField.m_id = -1;
    isLocallyModifiedField.m_required =
        Parser::Field::RequiredFlag::Required;
    isLocallyModifiedField.m_affiliation =
        Parser::Field::Affiliation::Local;
    isLocallyModifiedField.m_type =
        std::make_shared<Parser::PrimitiveType>(
            Parser::PrimitiveType::Type::Bool);
    isLocallyModifiedField.m_name =
        QStringLiteral("isLocallyModified");

    auto initializer = std::make_shared<Parser::LiteralValue>();
    initializer->m_value = QStringLiteral("false");
    isLocallyModifiedField.m_initializer = std::move(initializer);

    auto setterDefaultValue =
        std::make_shared<Parser::LiteralValue>();
    setterDefaultValue->m_value = QStringLiteral("true");
    isLocallyModifiedField.m_setterDefaultValue =
        std::move(setterDefaultValue);

    return LocalField{
        std::move(isLocallyModifiedField),
        QStringLiteral(
            "/**\n"
            " @brief locallyModified flag can be used to keep "
            "track which\n"
            "objects have been modified locally and thus need to "
            "be synchronized\n"
            " with Evernote service\n"
            "*/")};
}

LocalField generateLocalIdField()
{
    Parser::Field localIdField;
    localIdField.m_id = -1;
    localIdField.m_required =
        Parser::Field::RequiredFlag::Required;

    localIdField.m_affiliation =
        Parser::Field::Affiliation::Local;

    localIdField.m_type =
        std::make_shared<Parser::StringType>();

    localIdField.m_name = QStringLiteral("localId");

    return LocalField{
        std::move(localIdField),
        QStringLiteral(
            "/**\n"
            " @brief localId can be used as a local unique identifier\n"
            " for any data item before it has been synchronized with\n"
            " Evernote and thus before it can be identified using its "
            "guid.\n"
            "\n"
            " localId is generated automatically on\n"
            "construction for convenience but can be overridden "
            "manually\n"
            "*/")};
}

LocalField generateLinkedNotebookGuidField()
{
    Parser::Field linkedNotebookGuidField;
    linkedNotebookGuidField.m_id = -1;
    linkedNotebookGuidField.m_required =
        Parser::Field::RequiredFlag::Optional;

    linkedNotebookGuidField.m_affiliation =
        Parser::Field::Affiliation::Local;

    auto identifierType = std::make_shared<Parser::IdentifierType>();
    identifierType->m_identifier = QStringLiteral("Guid");
    linkedNotebookGuidField.m_type = std::move(identifierType);

    linkedNotebookGuidField.m_name = QStringLiteral("linkedNotebookGuid");

    return LocalField{
        std::move(linkedNotebookGuidField),
        QStringLiteral(
            "/**\n"
            " Guid of linked notebook which this notebook comes from. If\n"
            " this notebook belongs to user's own content i.e. doesn't\n"
            " come from any linked notebook, this field would be nullopt\n"
            "*/")};
}

LocalField generateParentTagLocalIdField()
{
    Parser::Field parentTagLocalIdField;
    parentTagLocalIdField.m_id = -1;
    parentTagLocalIdField.m_required =
        Parser::Field::RequiredFlag::Required;

    parentTagLocalIdField.m_affiliation =
        Parser::Field::Affiliation::Local;

    parentTagLocalIdField.m_type = std::make_shared<Parser::StringType>();
    parentTagLocalIdField.m_name = QStringLiteral("parentTagLocalId");

    return LocalField{
        std::move(parentTagLocalIdField),
        QStringLiteral(
            "/**\n"
            " Local id of a tag which is this tag's parent. Empty if this tag\n"
            " has no parent\n"
            "*/")};
}

LocalField generateNotebookLocalIdField()
{
    Parser::Field notebookLocalIdField;
    notebookLocalIdField.m_id = -1;
    notebookLocalIdField.m_required =
        Parser::Field::RequiredFlag::Required;

    notebookLocalIdField.m_affiliation =
        Parser::Field::Affiliation::Local;

    notebookLocalIdField.m_type =
        std::make_shared<Parser::StringType>();

    notebookLocalIdField.m_name = QStringLiteral("notebookLocalId");

    return LocalField{
        std::move(notebookLocalIdField),
        QStringLiteral(
            "/**\n"
            " Local id of a notebook to which this note belongs\n"
            "*/")};
}

LocalField generateTagLocalIdsField()
{
    Parser::Field tagLocalIdsField;
    tagLocalIdsField.m_id = -1;
    tagLocalIdsField.m_required =
        Parser::Field::RequiredFlag::Required;

    tagLocalIdsField.m_affiliation =
        Parser::Field::Affiliation::Local;

    auto listType = std::make_shared<Parser::ListType>();
    listType->m_valueType = std::make_shared<Parser::StringType>();
    tagLocalIdsField.m_type = std::move(listType);

    tagLocalIdsField.m_name = QStringLiteral("tagLocalIds");

    return LocalField{
        std::move(tagLocalIdsField),
        QStringLiteral(
            "/**\n"
            " Local ids of this note's tags\n"
            "*/")};
}

LocalField generateThumbnailDataField()
{
    Parser::Field thumbnailDataField;
    thumbnailDataField.m_id = -1;
    thumbnailDataField.m_required =
        Parser::Field::RequiredFlag::Required;

    thumbnailDataField.m_affiliation =
        Parser::Field::Affiliation::Local;

    thumbnailDataField.m_type = std::make_shared<Parser::ByteArrayType>();
    thumbnailDataField.m_name = QStringLiteral("thumbnailData");

    return LocalField{
        std::move(thumbnailDataField),
        QStringLiteral(
            "/**\n"
            " Thumbnail image data correspondng to the note\n"
            "*/")};
}

LocalField generateNoteLocalIdField()
{
    Parser::Field noteLocalIdField;
    noteLocalIdField.m_id = -1;
    noteLocalIdField.m_required =
        Parser::Field::RequiredFlag::Required;

    noteLocalIdField.m_affiliation =
        Parser::Field::Affiliation::Local;

    noteLocalIdField.m_type = std::make_shared<Parser::StringType>();
    noteLocalIdField.m_name = QStringLiteral("noteLocalId");

    return LocalField{
        std::move(noteLocalIdField),
        QStringLiteral(
            "/**\n"
            " Local id of a note to which this resource belongs\n"
            "*/")};
}

LocalField generateNoteGuidField()
{
    Parser::Field noteGuidField;
    noteGuidField.m_id = -1;
    noteGuidField.m_required =
        Parser::Field::RequiredFlag::Required;

    noteGuidField.m_affiliation =
        Parser::Field::Affiliation::Local;

    auto identifierType = std::make_shared<Parser::IdentifierType>();
    identifierType->m_identifier = QStringLiteral("Guid");
    noteGuidField.m_type = std::move(identifierType);

    noteGuidField.m_name = QStringLiteral("noteGuid");

    return LocalField{
        std::move(noteGuidField),
        QStringLiteral(
            "/**\n"
            " Guid of a note to which this shared note belongs\n"
            "*/")};
}

} // namespace qevercloud_generator
