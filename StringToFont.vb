Imports System
Imports System.IO
Imports System.Collections.Generic

' =========================== FONT DATA STRUCTURES ===========================

' Represents a single font character entry.
Public Class CharacterEntry
    Public Property Name As String
    Public Property UnicodeDecimal As String
    Public Property UnicodeOctal As String
    Public Property UnicodeHex As String
    Public Property UnicodeBinary As String
    Public Property Literal As String
    Public Property HtmlDecimal As String
    Public Property HtmlNamed As String
    Public Property Bitmap As List(Of Integer)
End Class

' Holds all character entries and provides lookup dictionaries for fast access.
Public Class CharacterMap
    Public CharacterEntries As New List(Of CharacterEntry)
    Public LookupByName As New Dictionary(Of String, CharacterEntry)
    Public LookupByLiteral As New Dictionary(Of String, CharacterEntry)
    Public LookupByUnicodeDecimal As New Dictionary(Of String, CharacterEntry)
    Public LookupByUnicodeOctal As New Dictionary(Of String, CharacterEntry)
    Public LookupByUnicodeHex As New Dictionary(Of String, CharacterEntry)
    Public LookupByUnicodeBinary As New Dictionary(Of String, CharacterEntry)
    Public LookupByHtmlDecimal As New Dictionary(Of String, CharacterEntry)
    Public LookupByHtmlNamed As New Dictionary(Of String, CharacterEntry)

    Public Sub BuildIndexes()
        For Each charEntry In CharacterEntries
            If Not String.IsNullOrEmpty(charEntry.Name) Then LookupByName(charEntry.Name) = charEntry
            If Not String.IsNullOrEmpty(charEntry.Literal) Then LookupByLiteral(charEntry.Literal) = charEntry
            If Not String.IsNullOrEmpty(charEntry.UnicodeDecimal) Then LookupByUnicodeDecimal(charEntry.UnicodeDecimal) = charEntry
            If Not String.IsNullOrEmpty(charEntry.UnicodeOctal) Then LookupByUnicodeOctal(charEntry.UnicodeOctal) = charEntry
            If Not String.IsNullOrEmpty(charEntry.UnicodeHex) Then LookupByUnicodeHex(charEntry.UnicodeHex) = charEntry
            If Not String.IsNullOrEmpty(charEntry.UnicodeBinary) Then LookupByUnicodeBinary(charEntry.UnicodeBinary) = charEntry
            If Not String.IsNullOrEmpty(charEntry.HtmlDecimal) Then LookupByHtmlDecimal(charEntry.HtmlDecimal) = charEntry
            If Not String.IsNullOrEmpty(charEntry.HtmlNamed) Then LookupByHtmlNamed(charEntry.HtmlNamed) = charEntry
        Next
    End Sub
End Class

' =========================== FONT LOADING ===========================

Public Module FontLoader

    Const FontRows As Integer = 7
    Const FontCols As Integer = 5
    Const BitmapStartLine As Integer = 8
    Const BitmapLength As Integer = FontRows * FontCols

    Public Function LoadCharacterMapFromFiles(fontFolderPath As String) As CharacterMap
        ' Reads all *.txt files in the given folder and builds a CharacterMap.
        Dim loadedCharMap As New CharacterMap()

        For Each charFilePath In Directory.GetFiles(fontFolderPath, "*.txt")
            Dim fileLines() As String = File.ReadAllLines(charFilePath)
            If fileLines.Length < FontRows Then Continue For

            Dim loadedEntry As New CharacterEntry With {
                .Name = fileLines(0),
                .UnicodeDecimal = fileLines(1),
                .UnicodeOctal = fileLines(2),
                .UnicodeHex = fileLines(3),
                .UnicodeBinary = fileLines(4),
                .Literal = fileLines(5),
                .HtmlDecimal = fileLines(6),
                .HtmlNamed = If(fileLines.Length > 7, fileLines(7), "")
            }

            ' Bitmap lines start at BitmapStartLine. Each line should be an integer.
            loadedEntry.Bitmap = New List(Of Integer)()
            For bitmapLineIdx As Integer = BitmapStartLine To Math.Min(fileLines.Length - 1, BitmapStartLine + BitmapLength - 1)
                Dim bitmapValue As Integer = 0
                Integer.TryParse(fileLines(bitmapLineIdx), bitmapValue)
                loadedEntry.Bitmap.Add(bitmapValue)
            Next

            ' Pad bitmap to BitmapLength elements if needed (FontCols x FontRows).
            While loadedEntry.Bitmap.Count < BitmapLength
                loadedEntry.Bitmap.Add(0)
            End While

            loadedCharMap.CharacterEntries.Add(loadedEntry)
        Next

        loadedCharMap.BuildIndexes()
        Return loadedCharMap
    End Function

End Module

' =========================== FONT RENDERING HELPERS ===========================

Public Module FontRenderer
    ' Render an individual character's 5x7 bitmap as a string array (one string per row)
    Public Function RenderFontToLines(character As CharacterEntry) As String()
        Dim renderedOutput As New List(Of String)
        For rowIdx As Integer = 0 To 6
            Dim rowStr As New System.Text.StringBuilder()
            For colIdx As Integer = 0 To 4
                Dim bitmapIdx As Integer = colIdx * 7 + rowIdx
                If bitmapIdx < character.Bitmap.Count AndAlso character.Bitmap(bitmapIdx) = 1 Then
                    rowStr.Append("#")
                Else
                    rowStr.Append(" ")
                End If
            Next
            renderedOutput.Add(rowStr.ToString())
        Next
        Return renderedOutput.ToArray()
    End Function

    ' Render a string to the console using the loaded font map
    Public Sub RenderStringToConsole(text As String, fontCharMap As CharacterMap, fallbackChar As CharacterEntry)
        Dim entriesToRender As New List(Of CharacterEntry)
        For Each inputChar As Char In text
            Dim foundEntry As CharacterEntry = Nothing
            If fontCharMap.LookupByLiteral.TryGetValue(inputChar.ToString(), foundEntry) Then
                entriesToRender.Add(foundEntry)
            Else
                entriesToRender.Add(fallbackChar)
            End If
        Next

        Dim renderedFontLinesPerChar As New List(Of String())()
        For Each renderEntry In entriesToRender
            renderedFontLinesPerChar.Add(RenderFontToLines(renderEntry))
        Next

        For rowIdx As Integer = 0 To 6
            For charIdx As Integer = 0 To renderedFontLinesPerChar.Count - 1
                Dim linesForChar = renderedFontLinesPerChar(charIdx)
                If rowIdx < linesForChar.Length Then
                    Console.Write(linesForChar(rowIdx).PadRight(5))
                Else
                    Console.Write("     ")
                End If
                Console.Write("  ")
            Next
            Console.WriteLine()
        Next
    End Sub
End Module



Public Module StringToFont

    Public Function BuildZoneFontPixelDictionary(
        zone As SpatialZone,   ' supplies FontSegmenter and WrappedTextQueue
        fontCharMap As CharacterMap   ' CharacterMap with loaded glyphs
    ) As Dictionary(Of (Integer, Integer), (List(Of (Integer, Integer)), List(Of (Integer, Integer))))

        Const FontPixelRows As Integer = 7
        Const FontPixelCols As Integer = 5

        ' Dictionary where (rowIdx, colIdx) maps to (offPixels, onPixels)
        Dim result As New Dictionary(Of (Integer, Integer), (List(Of (Integer, Integer)), List(Of (Integer, Integer))))
        Dim fallbackSlashEntry As CharacterEntry = Nothing
        If Not fontCharMap.LookupByName.TryGetValue("Slash", fallbackSlashEntry) Then
            Throw New Exception("Fallback 'Slash' character not found in font map!")
        End If

        ' Work with a copy of the queue so the original is not consumed
        Dim queueCopy As New Queue(Of String)(zone.WrappedTextQueue)
        Dim rowIdx As Integer = 0

        While queueCopy.Count > 0 AndAlso rowIdx < zone.FontSegmenter.SegmentRows
            Dim line As String = queueCopy.Dequeue()
            For colIdx As Integer = 0 To Math.Min(line.Length, zone.FontSegmenter.SegmentColumns) - 1
                Dim c As Char = line(colIdx)
                Dim entry As CharacterEntry = Nothing
                If Not fontCharMap.LookupByLiteral.TryGetValue(c.ToString(), entry) Then
                    entry = fallbackSlashEntry
                End If

                Dim seg = zone.FontSegmenter.GetSegment(rowIdx, colIdx)
                ' Each pixel coordinate is (panelRow, panelCol) in the overall panel grid.
                Dim offPixels As New List(Of (Integer, Integer))
                Dim onPixels As New List(Of (Integer, Integer))
                For pixelRow As Integer = 0 To FontPixelRows - 1
                    For pixelCol As Integer = 0 To FontPixelCols - 1
                        Dim bitmapIdx = pixelCol * FontPixelRows + pixelRow
                        Dim panelRow = seg.StartRow + pixelRow
                        Dim panelCol = seg.StartCol + pixelCol
                        If bitmapIdx < entry.Bitmap.Count AndAlso entry.Bitmap(bitmapIdx) = 1 Then
                            onPixels.Add((panelRow, panelCol))
                        Else
                            offPixels.Add((panelRow, panelCol))
                        End If
                    Next
                Next
                result((rowIdx, colIdx)) = (offPixels, onPixels)
            Next
            rowIdx += 1
        End While

        Return result
    End Function

End Module




















