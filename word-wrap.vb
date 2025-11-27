Imports System.Text
Imports System.Text.RegularExpressions

Public Module WordWrapConsole

    ' Main wrapping function: robust to edge cases and prevents out-of-range errors.

    Public Function WordWrapToQueue(
    userInputParagraphs As List(Of String),
    maximumCharactersPerLine As Integer,
    maximumLinesAllowedByUser As Integer ' <- keep for compatibility, but not used
) As Queue(Of String)
        Dim finalOutputLinesList As New List(Of String)
        For Each paragraphText As String In userInputParagraphs
            If paragraphText = "" Then
                finalOutputLinesList.Add("") ' Blank line for explicit newline
            Else
                Dim listOfTokensForParagraph As List(Of TextToken) = BreakWorkTextIntoTokens(paragraphText)
                listOfTokensForParagraph = HardWrapLongTokens(listOfTokensForParagraph, maximumCharactersPerLine)

                ' Defensive check: no token should exceed the allowed line length.
                Dim tooLong = listOfTokensForParagraph.FirstOrDefault(Function(tok) tok.Content.Length > maximumCharactersPerLine)
                If tooLong IsNot Nothing Then
                    Throw New InvalidOperationException(
                    $"Token '{tooLong.Content}' exceeds the maximum allowed line length ({maximumCharactersPerLine}) after hard-wrapping.")
                End If

                If listOfTokensForParagraph.Count = 0 Then
                    finalOutputLinesList.Add("") ' Entire line was whitespace
                Else
                    Dim wrappedLinesForParagraph As List(Of String) =
                    FindKnuthPlassOptimalWordWrap(listOfTokensForParagraph, maximumCharactersPerLine)
                    finalOutputLinesList.AddRange(wrappedLinesForParagraph)
                End If
            End If
        Next

        ' Return ALL output lines in a FIFO queue for paging.
        Return New Queue(Of String)(finalOutputLinesList)
    End Function
    Public Function WordWrapToRowLimitedQueue(
        userInputParagraphs As List(Of String),
        maximumCharactersPerLine As Integer,
        maximumLinesAllowedByUser As Integer
    ) As Queue(Of String)
        Dim finalOutputLinesList As New List(Of String)
        For Each paragraphText As String In userInputParagraphs
            If paragraphText = "" Then
                finalOutputLinesList.Add("") ' Blank line for explicit newline
            Else
                Dim listOfTokensForParagraph As List(Of TextToken) = BreakWorkTextIntoTokens(paragraphText)
                listOfTokensForParagraph = HardWrapLongTokens(listOfTokensForParagraph, maximumCharactersPerLine)

                ' Defensive check: no token should exceed the allowed line length.
                Dim tooLong = listOfTokensForParagraph.FirstOrDefault(Function(tok) tok.Content.Length > maximumCharactersPerLine)
                If tooLong IsNot Nothing Then
                    Throw New InvalidOperationException(
                        $"Token '{tooLong.Content}' exceeds the maximum allowed line length ({maximumCharactersPerLine}) after hard-wrapping.")
                End If

                If listOfTokensForParagraph.Count = 0 Then
                    finalOutputLinesList.Add("") ' Entire line was whitespace
                Else
                    Dim wrappedLinesForParagraph As List(Of String) =
                        FindKnuthPlassOptimalWordWrap(listOfTokensForParagraph, maximumCharactersPerLine)
                    finalOutputLinesList.AddRange(wrappedLinesForParagraph)
                End If
            End If
        Next

        ' Place all output lines in a FIFO queue for paging.
        Dim outputLinesQueue As New Queue(Of String)(finalOutputLinesList)

        ' Only keep up to the allowed number of lines.
        Dim limitedQueue As New Queue(Of String)()
        Dim count As Integer = 0
        While outputLinesQueue.Count > 0 AndAlso count < maximumLinesAllowedByUser
            limitedQueue.Enqueue(outputLinesQueue.Dequeue())
            count += 1
        End While

        Return limitedQueue
    End Function

#Region "Tokenization"

    Class TextToken
        Public Property Content As String
        Public Property IsSpace As Boolean
        Public Property IsPunctuation As Boolean
        Public Property IsSoftHyphen As Boolean
        Public Property IsBreakOpportunity As Boolean
        Public Sub New(content As String, Optional isSpace As Boolean = False, Optional isPunctuation As Boolean = False, Optional isSoftHyphen As Boolean = False, Optional isBreakOpportunity As Boolean = False)
            Me.Content = content
            Me.IsSpace = isSpace
            Me.IsPunctuation = isPunctuation
            Me.IsSoftHyphen = isSoftHyphen
            Me.IsBreakOpportunity = isBreakOpportunity
        End Sub
    End Class

    Function BreakWorkTextIntoTokens(paragraphText As String) As List(Of TextToken)
        Dim listOfTokens As New List(Of TextToken)
        Dim tokenPattern As String = "([\t ]+|[\p{P}]+|[\w]+|[\xAD])"
        Dim matches As MatchCollection = Regex.Matches(paragraphText, tokenPattern, RegexOptions.None)
        For Each match As Match In matches
            Dim tokenContent As String = match.Value
            If tokenContent = ChrW(&HAD) Then
                listOfTokens.Add(New TextToken("-", isSoftHyphen:=True, isBreakOpportunity:=True))
            ElseIf Regex.IsMatch(tokenContent, "^[\t ]+$") Then
                listOfTokens.Add(New TextToken(tokenContent, isSpace:=True, isBreakOpportunity:=True))
            ElseIf Regex.IsMatch(tokenContent, "^[\p{P}]+$") Then
                listOfTokens.Add(New TextToken(tokenContent, isPunctuation:=True, isBreakOpportunity:=False)) ' <-- Ensure this is False
            Else
                listOfTokens.Add(New TextToken(tokenContent))
            End If
        Next
        Return listOfTokens
    End Function

    Function HardWrapLongTokens(tokens As List(Of TextToken), maxLen As Integer) As List(Of TextToken)
        Dim result As New List(Of TextToken)
        For Each t In tokens
            Dim content As String = t.Content
            Dim startIdx As Integer = 0
            Do While startIdx < content.Length
                Dim remain As Integer = content.Length - startIdx
                If remain > maxLen Then
                    ' Only hyphenate if maxLen > 1, else forcibly break one char at a time (no hyphen)
                    If Not t.IsSpace AndAlso Not t.IsPunctuation AndAlso Not t.IsSoftHyphen AndAlso maxLen > 1 Then
                        Dim part As String = content.Substring(startIdx, maxLen - 1) & "-"
                        result.Add(New TextToken(part, False, False, False, True))
                        startIdx += (maxLen - 1)
                    Else
                        ' For space/punct/soft hyphen, or if maxLen <= 1: forcibly split, no hyphen
                        Dim part As String = content.Substring(startIdx, maxLen)
                        result.Add(New TextToken(part, t.IsSpace, t.IsPunctuation, t.IsSoftHyphen, True))
                        startIdx += maxLen
                    End If
                Else
                    ' Last piece: for words, no hyphen; for others, preserve flags, mark as break opp.
                    If Not t.IsSpace AndAlso Not t.IsPunctuation AndAlso Not t.IsSoftHyphen Then
                        Dim part As String = content.Substring(startIdx, remain)
                        result.Add(New TextToken(part, False, False, False, True))
                    Else
                        Dim part As String = content.Substring(startIdx, remain)
                        result.Add(New TextToken(part, t.IsSpace, t.IsPunctuation, t.IsSoftHyphen, True))
                    End If
                    Exit Do
                End If
            Loop
        Next
        Return result
    End Function

#End Region

    Function FindKnuthPlassOptimalWordWrap(listOfTokensForParagraph As List(Of TextToken), maximumCharactersPerLine As Integer) As List(Of String)
        ' This flag determines whether we relax the punctuation rule.
        ' If False (default): lines cannot start with punctuation except the first line.
        ' If True: allow lines to start with punctuation (fallback for edge cases).
        Dim relaxPunctuationRule As Boolean = False

RetryWrap: ' GOTO label for retrying with relaxed punctuation rule if needed

        If listOfTokensForParagraph Is Nothing OrElse listOfTokensForParagraph.Count = 0 Then
            Return New List(Of String) From {""}
        End If

        Dim numberOfTokens As Integer = listOfTokensForParagraph.Count
        ' A very large number acts as an initial "worst penalty" for comparison during optimization.
        Dim veryLargePenalty As Integer = Integer.MaxValue \ 2

        ' Construct break points where a line break is allowed.
        ' These are the token indices where we are allowed to break a line (e.g., after spaces or hyphens).
        Dim listOfBreakPoints As New List(Of Integer)
        listOfBreakPoints.Add(0)
        For tokenPosition As Integer = 1 To numberOfTokens
            If tokenPosition = numberOfTokens _
        OrElse listOfTokensForParagraph(tokenPosition - 1).IsBreakOpportunity Then
                listOfBreakPoints.Add(tokenPosition)
            End If
        Next

        Dim numberOfBreakPoints As Integer = listOfBreakPoints.Count

        ' These arrays are used for dynamic programming to track the minimum penalty ("cost") for each break point,
        ' and to record which next break point gives the optimal solution.
        Dim minimumPenaltyStartingAtBreakPoint(numberOfBreakPoints - 1) As Integer
        Dim nextBreakPointAfterCurrent(numberOfBreakPoints - 1) As Integer
        For eachBreakPointIndex As Integer = 0 To numberOfBreakPoints - 1
            minimumPenaltyStartingAtBreakPoint(eachBreakPointIndex) = veryLargePenalty
            nextBreakPointAfterCurrent(eachBreakPointIndex) = -1
        Next
        ' The penalty for the end of the paragraph is zero, since there is nothing left to wrap.
        minimumPenaltyStartingAtBreakPoint(numberOfBreakPoints - 1) = 0

        ' --- Main optimization loop: Try all possible line breaks in reverse order.
        ' DYNAMIC PROGRAMMING: 
        ' We start from the end of the paragraph and work backwards (right to left).
        ' For each break point i, we try all possible next break points j > i that don’t make the line too long.
        ' For each possibility, we compute the penalty for this line (raggedPenalty)
        ' plus the best penalty from the next break onward (minimumPenaltyStartingAtBreakPoint(j)).
        ' We keep the option with the lowest total penalty.
        For breakPointIndexToFill As Integer = numberOfBreakPoints - 2 To 0 Step -1
            For possibleNextBreakPointIndex As Integer = breakPointIndexToFill + 1 To numberOfBreakPoints - 1

                ' Enforce: Prevent a line from starting with punctuation (except for the first line in the paragraph)
                ' If relaxPunctuationRule is True, this check is skipped (fallback mode)
                If Not relaxPunctuationRule Then
                    Dim lineStartTokenIndex As Integer = listOfBreakPoints(breakPointIndexToFill)
                    If lineStartTokenIndex < listOfTokensForParagraph.Count _
                AndAlso listOfTokensForParagraph(lineStartTokenIndex).IsPunctuation _
                AndAlso breakPointIndexToFill <> 0 Then
                        Continue For
                    End If
                End If

                ' --- Compute the total length of this candidate line (from this break to the next)
                Dim totalLengthOfCurrentLine As Integer = 0
                For tokenIndexWithinLine As Integer = listOfBreakPoints(breakPointIndexToFill) To listOfBreakPoints(possibleNextBreakPointIndex) - 1
                    totalLengthOfCurrentLine += listOfTokensForParagraph(tokenIndexWithinLine).Content.Length
                Next
                ' If the line would be too long, stop checking further next breaks.
                If totalLengthOfCurrentLine > maximumCharactersPerLine Then Exit For

                ' --- Calculate the "raggedness" penalty for this line break:
                ' The goal is to keep the right edge as smooth as possible by penalizing unused space.
                Dim isThisTheLastLine As Boolean = (possibleNextBreakPointIndex = numberOfBreakPoints - 1)
                ' The number of unused character slots on this line.
                Dim unusedSpaceOnLine As Integer = maximumCharactersPerLine - totalLengthOfCurrentLine
                ' The penalty for raggedness: cube of unused spaces for all but the last line (which gets no penalty).
                ' This strongly discourages short/jagged lines except at the end of the paragraph.
                Dim raggedPenalty As Integer = If(isThisTheLastLine, 0, unusedSpaceOnLine * unusedSpaceOnLine * unusedSpaceOnLine)

                ' --- Total "cost" (penalty) of this line break and all following lines using optimal breaks.
                ' DYNAMIC PROGRAMMING:
                ' For each possible break, we add the penalty for this line (raggedPenalty) 
                ' to the already-computed best penalty for the next break (minimumPenaltyStartingAtBreakPoint(j)).
                ' This "reuse" of subproblem solutions is the essence of dynamic programming,
                ' avoiding recomputation and achieving efficiency.
                Dim overallPenalty As Integer = minimumPenaltyStartingAtBreakPoint(possibleNextBreakPointIndex) + raggedPenalty

                ' --- If this break results in a lower total penalty, record it as the best choice so far.
                If overallPenalty < minimumPenaltyStartingAtBreakPoint(breakPointIndexToFill) Then
                    minimumPenaltyStartingAtBreakPoint(breakPointIndexToFill) = overallPenalty
                    nextBreakPointAfterCurrent(breakPointIndexToFill) = possibleNextBreakPointIndex
                End If
            Next
        Next

        ' --- Backtrack through break points to reconstruct the sequence of optimal line breaks.
        ' DYNAMIC PROGRAMMING:
        ' After filling the arrays, we reconstruct the actual line breaks by following the array nextBreakPointAfterCurrent(),
        ' which tells us for each break point which “next” break point led to the optimal solution.
        ' In this way, we efficiently recover the best sequence of line breaks for the lowest total penalty.
        Dim wrappedLinesForParagraph As New List(Of String)
        Dim currentBreakPointIndex As Integer = 0
        While currentBreakPointIndex < numberOfBreakPoints - 1
            Dim nextBreakPointIndex As Integer = nextBreakPointAfterCurrent(currentBreakPointIndex)
            If nextBreakPointIndex = -1 Then
                ' If the strict punctuation rule is in effect, relax and retry
                If Not relaxPunctuationRule Then
                    relaxPunctuationRule = True
                    GoTo RetryWrap
                End If
                ' If already relaxed, print debug and throw exception as a last resort
                Console.WriteLine("=== DEBUG TOKEN LIST ===")
                For Each tok In listOfTokensForParagraph
                    Console.WriteLine($"Token: '{tok.Content}'  Length: {tok.Content.Length}  IsBreakOpportunity: {tok.IsBreakOpportunity}")
                Next
                Throw New InvalidOperationException(
            $"Word-wrap algorithm failed: could not find a valid line break starting at break point {currentBreakPointIndex}. " &
            $"This usually means a token is longer than the allowed line length or tokenization/hard-wrap failed.")
            End If
            wrappedLinesForParagraph.Add(BuildLineFromTokens(
        listOfTokensForParagraph,
        listOfBreakPoints(currentBreakPointIndex),
        listOfBreakPoints(nextBreakPointIndex),
        currentBreakPointIndex = 0 ' isFirstLine
    ))
            currentBreakPointIndex = nextBreakPointIndex
        End While
        Return wrappedLinesForParagraph
    End Function

    Function BuildLineFromTokens(listOfTokensForParagraph As List(Of TextToken), startTokenIndex As Integer, endTokenIndex As Integer, isFirstLine As Boolean) As String
        Dim lineBuilder As New StringBuilder()
        Dim idx As Integer = startTokenIndex
        ' ONLY skip a single leading space for lines that are NOT the first in the paragraph
        If Not isFirstLine AndAlso idx < endTokenIndex AndAlso listOfTokensForParagraph(idx).IsSpace Then
            idx += 1
        End If
        For tokenIndex As Integer = idx To endTokenIndex - 1
            lineBuilder.Append(listOfTokensForParagraph(tokenIndex).Content)
        Next
        Return lineBuilder.ToString()
    End Function


End Module