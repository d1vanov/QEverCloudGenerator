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

#pragma once

#include "Parser.h"

namespace qevercloud_generator {

[[nodiscard]] bool shouldGenerateLocalDataFields(const Parser::Structure & s);

// WARNING: This function assumes that the structure it is called on
// would yield shouldGenerateLocalDataFields(s) == true
[[nodiscard]] bool shouldGenerateLocalId(const Parser::Structure & s) noexcept;

struct LocalField
{
    Parser::Field m_field;
    QString m_docComment;
};

// "QHash<QString, QVariant> localData"
[[nodiscard]] LocalField generateLocalDataField();

// "bool isLocallyFavorited = false"
[[nodiscard]] LocalField generateLocallyFavoritedField();

// "bool isLocalOnly = false"
[[nodiscard]] LocalField generateLocalOnlyField();

// "bool isLocallyModified = false"
[[nodiscard]] LocalField generateLocallyModifiedField();

// "QString localId"
[[nodiscard]] LocalField generateLocalIdField();

// "std::optional<Guid> linkedNotebookGuid" - field for Notebook and Tag types
[[nodiscard]] LocalField generateLinkedNotebookGuidField();

// "QString parentTagLocalId" - field for Tag type
[[nodiscard]] LocalField generateParentTagLocalIdField();

// "QString notebookLocalId" - field for Note type
[[nodiscard]] LocalField generateNotebookLocalIdField();

// "QStringList tagLocalIds" - field for Note type
[[nodiscard]] LocalField generateTagLocalIdsField();

// "QByteArray thumbnailData" - field for Note type
[[nodiscard]] LocalField generateThumbnailDataField();

// "QString noteLocalId" - field for Resource type
[[nodiscard]] LocalField generateNoteLocalIdField();

// "std::optional<Guid> noteGuid" - field for SharedNote type
[[nodiscard]] LocalField generateNoteGuidField();

} // namespace qevercloud_generator
