Imports Current.PluginApi

Public Class SpatialZone

    Public Property BoundingBoxAABB As ((Integer, Integer, Integer), (Integer, Integer, Integer))
    Private _wrappedCharIndex As Dictionary(Of (Integer, Integer), Char)

    Public ReadOnly Property WrappedCharIndex As Dictionary(Of (Integer, Integer), Char)
        Get
            Return _wrappedCharIndex
        End Get
    End Property



    '–– at the top of your class, you already have these caches ––
    Private collectedOffPoints As New List(Of (Integer, Integer, Integer))
    Private collectedOffObjectIds As New List(Of Integer)

    ''' <summary>
    ''' Invert only the SECOND character cell (row 0, col 1).  
    ''' Flips ColorOverride on pre-created objects, then clears the temp lists.
    ''' </summary>
    Public Sub InvertColorsOn()
        ' 1) First revert any previous second‐cell inversion
        InvertColorsOff()

        ' 2) Identify the 2nd character's grid segment
        Dim seg = Me.FontSegmenter.GetSegment(0, 1)

        ' 3) Get that cell's OFF/ON pixel lists
        Dim cellData = Me.wholeZoneCellDict((seg.IndexRow, seg.IndexCol))
        Dim offPixels = cellData.Item1   ' background pixels
        Dim onPixels = cellData.Item2   ' character‐stroke pixels

        ' 4) Map (r,c) → world coords
        Dim grid = GetPanelGrid()
        Dim world As (Integer, Integer, Integer)

        ' 5) Override stroke (ON) pixels to black
        For Each rc In onPixels
            If grid.TryGetValue(rc, world) Then
                Dim idx = Me.collectedFontPoints.IndexOf(world)
                If idx >= 0 Then
                    Dim oid As Integer = Me.collectedFontObjectIds(idx)
                    Dim obj As MyObject = Nothing
                    objectDictionary.TryGetValue(oid, obj)
                    If obj IsNot Nothing Then obj.ColorOverride = ObjectColor.Black
                End If
            End If
        Next

        ' 6) Override background (OFF) pixels to yellow
        For Each rc In offPixels
            If grid.TryGetValue(rc, world) Then
                Dim idx = collectedOffPoints.IndexOf(world)
                If idx >= 0 Then
                    Dim oid As Integer = collectedOffObjectIds(idx)
                    Dim obj As MyObject = Nothing
                    objectDictionary.TryGetValue(oid, obj)
                    If obj IsNot Nothing Then obj.ColorOverride = ObjectColor.Yellow
                End If
            End If
        Next

        ' 7) Clear the temp caches so they don't accumulate
        collectedOffPoints.Clear()
        collectedOffObjectIds.Clear()
    End Sub

    ' Revert inversion on only the SECOND character cell (row 0, col 1).
    ' Clears any ColorOverride flags, then clears the temp lists.
    Public Sub InvertColorsOff()
        ' 1) Clear stroke‐pixel overrides on the 2nd character
        Dim seg = Me.FontSegmenter.GetSegment(0, 1)
        Dim onPixels = Me.wholeZoneCellDict((seg.IndexRow, seg.IndexCol)).Item2
        Dim grid = GetPanelGrid()
        Dim world As (Integer, Integer, Integer)

        For Each rc In onPixels
            If grid.TryGetValue(rc, world) Then
                Dim idx = Me.collectedFontPoints.IndexOf(world)
                If idx >= 0 Then
                    Dim oid As Integer = Me.collectedFontObjectIds(idx)
                    Dim obj As MyObject = Nothing
                    objectDictionary.TryGetValue(oid, obj)
                    If obj IsNot Nothing Then obj.ColorOverride = Nothing
                End If
            End If
        Next

        ' 2) Clear background‐pixel overrides
        For Each rc In Me.wholeZoneCellDict((seg.IndexRow, seg.IndexCol)).Item1
            If grid.TryGetValue(rc, world) Then
                Dim idx = collectedOffPoints.IndexOf(world)
                If idx >= 0 Then
                    Dim oid As Integer = collectedOffObjectIds(idx)
                    Dim obj As MyObject = Nothing
                    objectDictionary.TryGetValue(oid, obj)
                    If obj IsNot Nothing Then obj.ColorOverride = Nothing
                End If
            End If
        Next

        ' 3) Clear the temp caches so they don't accumulate
        collectedOffPoints.Clear()
        collectedOffObjectIds.Clear()
    End Sub
















    '=======================
    Public Sub SetGutterVisible(row As Integer, col As Integer, side As String, visible As Boolean)
        Dim seg = Me.FontSegmenter.GetSegment(row, col)
        Dim above As List(Of (Integer, Integer)) = Nothing
        Dim below As List(Of (Integer, Integer)) = Nothing
        Dim left As List(Of (Integer, Integer)) = Nothing
        Dim right As List(Of (Integer, Integer)) = Nothing
        Me.FontSegmenter.GetSurroundingStrips(seg, above, below, left, right)
        Dim gutterCells As List(Of (Integer, Integer)) = Nothing
        Select Case side.ToLower()
            Case "above"
                gutterCells = above
            Case "below"
                gutterCells = below
            Case "left"
                gutterCells = left
            Case "right"
                gutterCells = right
            Case Else
                Throw New ArgumentException("Invalid side: must be 'above', 'below', 'left', or 'right'")
        End Select

        ' Choose the panel grid
        Dim panelGrid = GetPanelGrid()

        For Each cell In gutterCells
            If panelGrid.ContainsKey(cell) Then
                Dim xyz = panelGrid(cell)
                If visible Then
                    ' Add if not already present
                    If Not collectedGutterPoints.Contains((xyz.Item1, xyz.Item2, xyz.Item3)) Then
                        Dim objId As Integer = AddMyObjectToFactory(xyz.Item1, xyz.Item2, xyz.Item3, gutterStructureId)
                        collectedGutterPoints.Add((xyz.Item1, xyz.Item2, xyz.Item3))
                        collectedGutterObjectIds.Add(objId)
                    End If
                Else
                    ' Remove if present
                    If collectedGutterPoints.Contains((xyz.Item1, xyz.Item2, xyz.Item3)) Then
                        Dim idx = collectedGutterPoints.IndexOf((xyz.Item1, xyz.Item2, xyz.Item3))
                        If idx >= 0 Then
                            Dim objId = collectedGutterObjectIds(idx)
                            objectDictionary.TryRemove(objId, Nothing)
                            collectedGutterObjectIds.RemoveAt(idx)
                            collectedGutterPoints.RemoveAt(idx)
                        End If
                    End If
                End If
            End If
        Next
    End Sub

    '=====================


    ' ------------------------------------------------------------------
    ' Helper: returns the correct panel‐grid for the current panelCommon
    ' ------------------------------------------------------------------
    Private Function GetPanelGrid() As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
        Select Case panelCommon
            Case PanelType.TopPanel
                Return panelData.TopPanelGrid
            Case PanelType.BottomPanel
                Return panelData.BottomPanelGrid
            Case PanelType.NorthPanel
                Return panelData.NorthPanelGrid
            Case PanelType.SouthPanel
                Return panelData.SouthPanelGrid
            Case PanelType.EastPanel
                Return panelData.EastPanelGrid
            Case PanelType.WestPanel
                Return panelData.WestPanelGrid
            Case Else
                Throw New InvalidOperationException(
                    $"Unknown panel type: {panelCommon}")
        End Select
    End Function














    '======================



    Public panelCommon As PanelType ' The panel this zone is mapped to (must be the same for all 4 margins)

    'Private marginMgr As Margins.MarginManager ' CAN I COMMENT THIS?

    Private collisionTriangleSetId As Integer = 0
    Private topLeftCornerCartesian As (Integer, Integer, Integer)
    Private topRightCornerCartesian As (Integer, Integer, Integer)
    Private bottomLeftCornerCartesian As (Integer, Integer, Integer)
    Private bottomRightCornerCartesian As (Integer, Integer, Integer)
    Public Sub UpdateCollisionTriangles()
        If collisionTriangleSetId > 0 Then
            RemoveAllTrianglesInSet(collisionTriangleSetId)
        End If
        If collisionTriangleSetId = 0 Then
            collisionTriangleSetId = GetNextUniqId()
        End If
        Dim a = topLeftCornerCartesian
        Dim b = topRightCornerCartesian
        Dim c = bottomRightCornerCartesian
        Dim d = bottomLeftCornerCartesian
        AddTriangle(a.Item1, a.Item2, a.Item3, b.Item1, b.Item2, b.Item3, c.Item1, c.Item2, c.Item3, collisionTriangleSetId)
        AddTriangle(a.Item1, a.Item2, a.Item3, c.Item1, c.Item2, c.Item3, d.Item1, d.Item2, d.Item3, collisionTriangleSetId)
    End Sub

    Public Sub RemoveCollisionTriangles()
        If collisionTriangleSetId > 0 Then
            RemoveAllTrianglesInSet(collisionTriangleSetId)
        End If
    End Sub


    ' Example usage, replacing old SwapMargenSet* methods:
    Sub SwapMargenSetNorth()
        UpdateMargins("NorthSetLeft", "NorthSetRight", "NorthSetTop", "NorthSetBottom")
    End Sub
    Sub SwapMargenSetEast()
        UpdateMargins("EastSetLeft", "EastSetRight", "EastSetTop", "EastSetBottom")
    End Sub
    Sub SwapMargenSetSouth()
        UpdateMargins("SouthSetLeft", "SouthSetRight", "SouthSetTop", "SouthSetBottom")
    End Sub
    Sub SwapMargenSetWest()
        UpdateMargins("WestSetLeft", "WestSetRight", "WestSetTop", "WestSetBottom")
    End Sub
    Sub SwapMargenSetTop()
        UpdateMargins("TopSetLeft", "TopSetRight", "TopSetTop", "TopSetBottom")
    End Sub
    Sub SwapMargenSetBottom()
        UpdateMargins("BottomSetLeft", "BottomSetRight", "BottomSetTop", "BottomSetBottom")
    End Sub

    ' Unified method to update margins and recalculate the zone
    Private leftColumnMarginId As String
    Private rightColumnMarginId As String
    Private topRowMarginId As String
    Private bottomRowMarginId As String
    Public Sub UpdateMargins(leftId As String, rightId As String, topId As String, bottomId As String)
        RemoveAllZoneObjects() '* instead use DisposeZone()
        leftColumnMarginId = leftId
        rightColumnMarginId = rightId
        topRowMarginId = topId
        bottomRowMarginId = bottomId

        leftColumnIndex = marginMgr.GetMarginInfo(leftColumnMarginId)("Column")
        rightColumnIndex = marginMgr.GetMarginInfo(rightColumnMarginId)("Column")
        topRowIndex = marginMgr.GetMarginInfo(topRowMarginId)("Row")
        bottomRowIndex = marginMgr.GetMarginInfo(bottomRowMarginId)("Row")

        'Console.WriteLine($"szLeft: {leftSide}, szRight: {rightSide}, szTop: {topSide}, szBottom: {bottomSide}")
        MapAndObject()
    End Sub

    ' Public property for IntelliSense/discoverability
    Public Property Text As String
        Get
            Return _text
        End Get
        Set(value As String)
            If _text <> value Then
                _text = value
                ' If margins are already set, redraw zone with new text
                If leftColumnMarginId IsNot Nothing AndAlso rightColumnMarginId IsNot Nothing AndAlso topRowMarginId IsNot Nothing AndAlso bottomRowMarginId IsNot Nothing Then
                    UpdateMargins(leftColumnMarginId, rightColumnMarginId, topRowMarginId, bottomRowMarginId)
                End If
            End If
        End Set
    End Property

    ' Create a SpatialZone by margin IDs (usually "1", "2", "3", "4" for left, right, top, bottom)
    Public Property ID As String
    ' Store the current text (use a private backing field)
    Private _text As String = ""
    Public Sub New(szId As String, mManager As Margins.MarginManager)
        ID = szId
        marginMgr = mManager

        'SwapMargenSetNorth() ' default position for meow
        AssignMarginSetA("DefaultTopSet")
        AssignMarginSetB("DefaultBottomSet")
        SwitchToMarginSetA()

        Dim sw As New Stopwatch()
        sw.Start()
        InitializeSortedSetIds()
        sw.Stop()
        Dim ticks As Long = sw.ElapsedTicks
        Dim nanosecondsPerTick As Double = (1000000000.0 / Stopwatch.Frequency)
        Dim elapsedNanoseconds As Double = ticks * nanosecondsPerTick
        Dim elapsedMilliseconds As Double = elapsedNanoseconds / 1000000.0
        'Console.WriteLine("InitializeSortedSetIds time: " & elapsedMilliseconds & " ms")
    End Sub

    ' IDs for each spatial zone element type
    Public borderStructureId As Integer
    Public gutterStructureId As Integer
    Public fontStructureId As Integer
    Public Sub SendBorderObjectsToFactory() ' When sending objects to the factory, record the returned object ID:
        borderStructureId = 11001
        For Each pt In collectedBorderPoints
            Dim objId As Integer = AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, borderStructureId)
            collectedBorderObjectIds.Add(objId)
        Next
    End Sub
    Public Sub SendGutterObjectsToFactory()
        gutterStructureId = 11002
        For Each pt In collectedGutterPoints
            Dim objId As Integer = AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, gutterStructureId)
            collectedGutterObjectIds.Add(objId)
        Next
    End Sub
    Public Sub SendFontObjectsToFactory()
        fontStructureId = 11003
        For Each pt In collectedFontPoints
            Dim objId As Integer = AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, fontStructureId)
            collectedFontObjectIds.Add(objId)
        Next
    End Sub

    Public collectedBorderPoints As New List(Of (Integer, Integer, Integer)) ' before use
    Public collectedGutterPoints As New List(Of (Integer, Integer, Integer))
    Public collectedFontPoints As New List(Of (Integer, Integer, Integer))
    Public collectedBorderObjectIds As New List(Of Integer) ' after use
    Public collectedGutterObjectIds As New List(Of Integer)
    Public collectedFontObjectIds As New List(Of Integer)
    Public Sub RemoveAllZoneObjects()
        For Each objId In collectedBorderObjectIds
            objectDictionary.TryRemove(objId, Nothing) ' IS THERE another Dict that holds the structure ID?
        Next
        For Each objId In collectedGutterObjectIds
            objectDictionary.TryRemove(objId, Nothing)
        Next
        For Each objId In collectedFontObjectIds
            objectDictionary.TryRemove(objId, Nothing)
        Next
        collectedBorderObjectIds.Clear()
        collectedGutterObjectIds.Clear()
        collectedFontObjectIds.Clear()
        collectedBorderPoints.Clear()
        collectedGutterPoints.Clear()
        collectedFontPoints.Clear()
    End Sub
    'Public Sub DisposeZone() ' Cleans up all the objects/resources that zone managed
    '    ' Remove all created objects from Program.vb global containers
    '    If borderStructureId > 0 Then RemoveObjectsByStructureId(borderStructureId)
    '    If gutterStructureId > 0 Then RemoveObjectsByStructureId(gutterStructureId)
    '    If fontStructureId > 0 Then RemoveObjectsByStructureId(fontStructureId)
    '    ' Remove any other references you keep elsewhere
    '    collectedBorderObjectIds.Clear()
    '    collectedGutterObjectIds.Clear()
    '    collectedFontObjectIds.Clear()
    '    collectedBorderPoints.Clear()
    '    collectedGutterPoints.Clear()
    '    collectedFontPoints.Clear()
    'End Sub

    Public Property FontSegmenter As GridFontSegmenter ' FontSegmenter provides layout for maximal 5x7 cells in this zone
    Public WrappedTextQueue As Queue(Of String)
    Public wholeZoneCellDict As Dictionary(Of (Integer, Integer), (List(Of (Integer, Integer)), List(Of (Integer, Integer))))
    Public Sub ProduceFontInGrid()
        ' Default (bordered mode):
        'FontSegmenter = New GridFontSegmenter(Me)
        ' Fullscreen mode:
        'FontSegmenter = New GridFontSegmenter(Me, fullscreenMode:=True)

        ' Example: your text (could come from anywhere)
        Dim inputText2 As String = "Hello world!  This is a   test." & vbCrLf & vbCrLf & "Second paragraph,  with tabs	and  spaces."


        ' Use the .Text property as the source for the displayed string
        Dim inputText As String = Me.Text
        ' Prepare the input as a list of lines, preserving all blank lines and whitespace
        Dim inputLines As List(Of String) = inputText.Split({vbCrLf, vbLf, vbCr}, StringSplitOptions.None).ToList()

        ' Use your segment grid as limits
        Dim segCols As Integer = FontSegmenter.SegmentColumns ' see comment above. we are not making new so how to FS?
        Dim segRows As Integer = FontSegmenter.SegmentRows    ' ** FS could also just give a window an extra pixel per side.

        WrappedTextQueue = WordWrapToRowLimitedQueue(
        inputLines,      ' ^^ use WordWrapToQueue if you want all lines from input text
        segCols,         ' characters per line
        segRows)          ' number of lines     

        Dim fontFolderPath As String = "C:\Users\Administrator\mc\plugins\CommandHelper\notes\Font5x7"
        Dim fontCharMap As CharacterMap = LoadCharacterMapFromFiles(fontFolderPath)

        wholeZoneCellDict = BuildZoneFontPixelDictionary(Me, fontCharMap)

        ' Loop through every cell in the spatial zone
        For Each cellKey In wholeZoneCellDict.Keys
            Dim ones = wholeZoneCellDict(cellKey).Item2  ' Item2 is the ON (white) pixel list

            ' Select the correct panel grid for this zone's panel
            Dim panelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer)) = GetPanelGrid()

            If panelGrid IsNot Nothing Then
                For Each rc In ones
                    If panelGrid.ContainsKey(rc) Then
                        Dim xyz As (Integer, Integer, Integer) = panelGrid(rc)
                        collectedFontPoints.Add((xyz.Item1, xyz.Item2, xyz.Item3))
                    End If
                Next
            End If

        Next
        ' --------------------------------------------------
        '  STEP 2: Build the per‐cell wrapped‐text character index
        '  This runs immediately after you set WrappedTextQueue
        _wrappedCharIndex = New Dictionary(Of (Integer, Integer), Char)()
        Dim wrappedLines() As String = WrappedTextQueue.ToArray()
        For r As Integer = 0 To wrappedLines.Length - 1
            Dim line As String = wrappedLines(r)
            For c As Integer = 0 To line.Length - 1
                _wrappedCharIndex((r, c)) = line(c)
            Next
        Next
        ' --------------------------------------------------

    End Sub


    ' The four margin indices (column or row numbers) defining the rectangle within the panel.
    Private leftColumnIndex As Integer
    Private rightColumnIndex As Integer
    Private topRowIndex As Integer
    Private bottomRowIndex As Integer
    Public ReadOnly Property LeftColumn As Integer    ' Expose zone's bounds for use by GridFontSegmenter and others
        Get
            Return leftColumnIndex
        End Get
    End Property
    Public ReadOnly Property RightColumn As Integer
        Get
            Return rightColumnIndex
        End Get
    End Property
    Public ReadOnly Property TopRow As Integer
        Get
            Return topRowIndex
        End Get
    End Property
    Public ReadOnly Property BottomRow As Integer
        Get
            Return bottomRowIndex
        End Get
    End Property

    ' Helper: Return a list of points (row, col) between two corners (exclusive), horizontal or vertical only.
    Private Function GetPointsBetween(startPt As (Integer, Integer), endPt As (Integer, Integer)) As List(Of (Integer, Integer))
        Dim points As New List(Of (Integer, Integer))()
        If startPt.Item2 = endPt.Item2 Then
            Dim col = startPt.Item2
            Dim rowStart = Math.Min(startPt.Item1, endPt.Item1) + 1
            Dim rowEnd = Math.Max(startPt.Item1, endPt.Item1) - 1
            For row = rowStart To rowEnd
                points.Add((row, col))
            Next
        ElseIf startPt.Item1 = endPt.Item1 Then
            Dim row = startPt.Item1
            Dim colStart = Math.Min(startPt.Item2, endPt.Item2) + 1
            Dim colEnd = Math.Max(startPt.Item2, endPt.Item2) - 1
            For col = colStart To colEnd
                points.Add((row, col))
            Next
        End If
        Return points
    End Function

    ' Checks that margins are in the correct order and all on the same panel.
    Function mapCheck() As Boolean
        Dim isLeftLT_RIght As Boolean = Me.leftColumnIndex < Me.rightColumnIndex
        Dim isTopLT_Bottom As Boolean = Me.topRowIndex < Me.bottomRowIndex
        'Console.WriteLine($"Is left < right? {isLeftLT_RIght}")
        'Console.WriteLine($"Is top < bottom? {isTopLT_Bottom}")

        Dim result As Boolean = False

        If isLeftLT_RIght AndAlso isTopLT_Bottom Then
            'Console.WriteLine("Both left < right AND top < bottom.  -PASS")

            Dim panelOfLeftMargin As PanelType = marginMgr.GetMarginInfo(leftColumnMarginId)("Panel")
            Dim panelOfRightMargin As PanelType = marginMgr.GetMarginInfo(rightColumnMarginId)("Panel")
            Dim panelOfTopMargin As PanelType = marginMgr.GetMarginInfo(topRowMarginId)("Panel")
            Dim panelOfBottomMargin As PanelType = marginMgr.GetMarginInfo(bottomRowMarginId)("Panel")

            Dim allOnSamePanel As Boolean = panelOfLeftMargin = panelOfRightMargin AndAlso panelOfLeftMargin = panelOfTopMargin AndAlso panelOfLeftMargin = panelOfBottomMargin

            If allOnSamePanel Then
                panelCommon = panelOfLeftMargin
                'Console.WriteLine($"All sides are on panel: {panelCommon}")
                result = True
            End If
        ElseIf isLeftLT_RIght Then
            'Console.WriteLine("Only left < right.  -NO PASS")
        ElseIf isTopLT_Bottom Then
            'Console.WriteLine("Only top < bottom.  -NO PASS")
        Else
            'Console.WriteLine("Neither left < right nor top < bottom.  -NO PASS")
        End If

        Return result
    End Function

    ' Rectangle corners in (row, col) notation (panel grid indices)
    Private topLeftCorner As (Integer, Integer)
    Private bottomLeftCorner As (Integer, Integer)
    Private topRightCorner As (Integer, Integer)
    Private bottomRightCorner As (Integer, Integer)
    ' Lists of points along each border (excluding the corners themselves)
    Private borderLeftPoints As List(Of (Integer, Integer))
    Private borderRightPoints As List(Of (Integer, Integer))
    Private borderTopPoints As List(Of (Integer, Integer))
    Private borderBottomPoints As List(Of (Integer, Integer))
    Sub MapAndObject() ' Maps the corners and borders of the zone to 3D coordinates and adds objects for visualization.  
        If mapCheck() Then
            'Console.WriteLine("Map check passed: all conditions are satisfied.")
        Else
            BoundingBoxAABB = ComputeBoundingBoxAABB()
            RemoveCollisionTriangles()
            Console.WriteLine("Map check failed: one or more conditions are not satisfied.")
            Return
        End If

        ' Assign corners (row, col)
        topLeftCorner = (topRowIndex, leftColumnIndex)
        bottomLeftCorner = (bottomRowIndex, leftColumnIndex)
        topRightCorner = (topRowIndex, rightColumnIndex)
        bottomRightCorner = (bottomRowIndex, rightColumnIndex)
        'Console.WriteLine($"TopLeft: {topLeftCorner}, BottomLeft: {bottomLeftCorner}, TopRight: {topRightCorner}, BottomRight: {bottomRightCorner}")

        ' --- First refactoring: get the panel‐grid once for all corner lookups ---
        Dim grid = GetPanelGrid()
        topLeftCornerCartesian = grid(topLeftCorner)
        topRightCornerCartesian = grid(topRightCorner)
        bottomLeftCornerCartesian = grid(bottomLeftCorner)
        bottomRightCornerCartesian = grid(bottomRightCorner)

        collectedBorderPoints.Add((topLeftCornerCartesian.Item1, topLeftCornerCartesian.Item2, topLeftCornerCartesian.Item3))
        collectedBorderPoints.Add((topRightCornerCartesian.Item1, topRightCornerCartesian.Item2, topRightCornerCartesian.Item3))
        collectedBorderPoints.Add((bottomLeftCornerCartesian.Item1, bottomLeftCornerCartesian.Item2, bottomLeftCornerCartesian.Item3))
        collectedBorderPoints.Add((bottomRightCornerCartesian.Item1, bottomRightCornerCartesian.Item2, bottomRightCornerCartesian.Item3))

        ' Build border point lists (excluding corners)
        borderLeftPoints = GetPointsBetween(topLeftCorner, bottomLeftCorner)
        borderRightPoints = GetPointsBetween(topRightCorner, bottomRightCorner)
        borderTopPoints = GetPointsBetween(topLeftCorner, topRightCorner)
        borderBottomPoints = GetPointsBetween(bottomLeftCorner, bottomRightCorner)

        Dim borderLists As List(Of List(Of (Integer, Integer))) = New List(Of List(Of (Integer, Integer))) From {
            borderLeftPoints, borderRightPoints, borderTopPoints, borderBottomPoints
        }

        ' --- Second refactoring: reuse the same grid for all border points ---
        For Each border In borderLists
            For Each pt In border
                Dim xyz = grid(pt)
                ' AddMyObjectToFactory(xyz.Item1, xyz.Item2, xyz.Item3)
                collectedBorderPoints.Add((xyz.Item1, xyz.Item2, xyz.Item3))
            Next
        Next

        ' Ensure FontSegmenter is instantiated ONCE, here in MapAndObject:
        FontSegmenter = New GridFontSegmenter(Me)

        ' Check if space is available for at least one full font segment + its gutters
        Dim segCols As Integer = FontSegmenter.SegmentColumns
        Dim segRows As Integer = FontSegmenter.SegmentRows
        Dim segmenterHasSpace As Boolean = (segCols >= 1 AndAlso segRows >= 1)
        Dim reqWidth As Integer = GridFontSegmenter.SegmentWidth + 2 * GridFontSegmenter.Strip
        Dim reqHeight As Integer = GridFontSegmenter.SegmentHeight + 2 * GridFontSegmenter.Strip
        Dim actualWidth As Integer = rightColumnIndex - leftColumnIndex + 1
        Dim actualHeight As Integer = bottomRowIndex - topRowIndex + 1
        Dim fitsGutteredCell As Boolean = (actualWidth >= reqWidth AndAlso actualHeight >= reqHeight)

        'Console.WriteLine($"Zone size: {actualWidth}x{actualHeight}, required for 1 cell+gutters: {reqWidth}x{reqHeight}")
        'Console.WriteLine($"Segment columns: {segCols}, Segment rows: {segRows}, segmenterHasSpace: {segmenterHasSpace}, fitsGutteredCell: {fitsGutteredCell}")

        If segmenterHasSpace AndAlso fitsGutteredCell Then
            'Console.WriteLine("Drawing font and gutters.")
            ProduceFontInGrid()
            'AddAllGuttersToFactory()
            SendFontObjectsToFactory()
        Else
            Console.WriteLine("Zone too small for font segment and gutters; only drawing border.") 'what if we are *fullscreen mode*?
        End If

        SendBorderObjectsToFactory()
        UpdateCollisionTriangles()

        BoundingBoxAABB = ComputeBoundingBoxAABB()
    End Sub
    Private Function ComputeBoundingBoxAABB() As ((Integer, Integer, Integer), (Integer, Integer, Integer))
        Dim xs = {topLeftCornerCartesian.Item1, topRightCornerCartesian.Item1, bottomRightCornerCartesian.Item1, bottomLeftCornerCartesian.Item1}
        Dim ys = {topLeftCornerCartesian.Item2, topRightCornerCartesian.Item2, bottomRightCornerCartesian.Item2, bottomLeftCornerCartesian.Item2}
        Dim zs = {topLeftCornerCartesian.Item3, topRightCornerCartesian.Item3, bottomRightCornerCartesian.Item3, bottomLeftCornerCartesian.Item3}
        Return ((xs.Min, ys.Min, zs.Min), (xs.Max, ys.Max, zs.Max))
    End Function

    Private Sub AddAllGuttersToFactory()
        ' Loop through all 5x7 segments within this zone
        Dim grid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer)) = GetPanelGrid()

        For Each seg In FontSegmenter.AllSegments()
            ' Initialize ByRef variables to Nothing before calling GetSurroundingStrips
            Dim above As List(Of (Integer, Integer)) = Nothing
            Dim below As List(Of (Integer, Integer)) = Nothing
            Dim left As List(Of (Integer, Integer)) = Nothing
            Dim right As List(Of (Integer, Integer)) = Nothing
            FontSegmenter.GetSurroundingStrips(seg, above, below, left, right)

            ' Helper function to map (row, col) to (x, y, z) and add to factory
            Dim addGutterCells As Action(Of List(Of (Integer, Integer))) =
            Sub(cells)
                For Each cell In cells
                    Dim xyz As (Integer, Integer, Integer) = grid(cell)
                    'AddMyObjectToFactory(xyz.Item1, xyz.Item2, xyz.Item3)
                    collectedGutterPoints.Add((xyz.Item1, xyz.Item2, xyz.Item3))
                Next
            End Sub

            ' Process each gutter
            addGutterCells(above)
            addGutterCells(below)
            addGutterCells(left)
            addGutterCells(right)
        Next

        SendGutterObjectsToFactory()
    End Sub



















    ' Two named slots, A and B, that hold margin set names.
    Private marginSetAName As String = Nothing
    Private marginSetBName As String = Nothing

    ' Tracks which slot is currently active/displayed. Valid values: "A", "B", or Nothing.
    Private currentMarginSlot As String = Nothing

    ' ===========================
    '  Core helpers
    ' ===========================

    ' Internal helper: validates a set exists and returns its dict.
    Private Function GetValidatedMarginSet(setName As String) As Dictionary(Of String, String)
        Dim setDict = marginMgr.GetMarginSet(setName)
        If setDict Is Nothing Then
            Throw New Exception("Margin set not found: " & setName)
        End If
        Return setDict
    End Function

    ' Internal helper: apply by margin-set name (no slot semantics here).
    Private Sub ApplyMarginSet(setName As String)
        Dim setDict = GetValidatedMarginSet(setName)
        Me.UpdateMargins(
            setDict("LeftColumn"),
            setDict("RightColumn"),
            setDict("TopRow"),
            setDict("BottomRow")
        )
    End Sub

    ' ===========================
    '  Public slot assignment
    ' ===========================

    ' Assigns a margin set to slot A, optionally makes it the active slot immediately.
    Public Sub AssignMarginSetA(setName As String, Optional makeActive As Boolean = False)
        GetValidatedMarginSet(setName) ' throws if not found
        marginSetAName = setName
        If makeActive Then
            SwitchToMarginSetA()
        End If
    End Sub

    ' Assigns a margin set to slot B, optionally makes it the active slot immediately.
    Public Sub AssignMarginSetB(setName As String, Optional makeActive As Boolean = False)
        GetValidatedMarginSet(setName) ' throws if not found
        marginSetBName = setName
        If makeActive Then
            SwitchToMarginSetB()
        End If
    End Sub

    ' ===========================
    '  Public slot switching
    ' ===========================

    ' Force switch to slot A, regardless of which slot is currently active.
    Public Sub SwitchToMarginSetA()
        If String.IsNullOrEmpty(marginSetAName) Then
            Throw New InvalidOperationException("Margin Set A is not assigned for zone " & ID)
        End If
        ApplyMarginSet(marginSetAName)
        currentMarginSlot = "A"
    End Sub

    ' Force switch to slot B, regardless of which slot is currently active.
    Public Sub SwitchToMarginSetB()
        If String.IsNullOrEmpty(marginSetBName) Then
            Throw New InvalidOperationException("Margin Set B is not assigned for zone " & ID)
        End If
        ApplyMarginSet(marginSetBName)
        currentMarginSlot = "B"
    End Sub

    ' Legacy-named but now strictly A/B-based: swap between slot A and B.
    ' If current slot is A, swap to B; if B, swap to A.
    ' If no current slot but both A and B exist, default to A.
    Public Sub SwapToAlternateMarginSet()
        ' If either A or B is missing, there's nothing sensible to swap.
        If String.IsNullOrEmpty(marginSetAName) OrElse String.IsNullOrEmpty(marginSetBName) Then
            Exit Sub
        End If

        Select Case currentMarginSlot
            Case "A"
                SwitchToMarginSetB()
            Case "B"
                SwitchToMarginSetA()
            Case Else
                ' No active slot yet; default to A
                SwitchToMarginSetA()
        End Select
    End Sub

    ' Direct assignment by name (kept as an internal convenience API if you still want it).
    ' If this name equals A or B, we switch to that slot; otherwise we just apply it and clear slot tracking.
    Public Sub AssignMarginSet(setName As String)
        Dim setDict = GetValidatedMarginSet(setName)
        Me.UpdateMargins(
            setDict("LeftColumn"),
            setDict("RightColumn"),
            setDict("TopRow"),
            setDict("BottomRow")
        )

        ' Update currentMarginSlot if it matches A/B.
        If marginSetAName IsNot Nothing AndAlso String.Equals(marginSetAName, setName, StringComparison.OrdinalIgnoreCase) Then
            currentMarginSlot = "A"
        ElseIf marginSetBName IsNot Nothing AndAlso String.Equals(marginSetBName, setName, StringComparison.OrdinalIgnoreCase) Then
            currentMarginSlot = "B"
        Else
            currentMarginSlot = Nothing
        End If
    End Sub

    ' Returns the name of the currently active slot's margin set, or Nothing.
    Public Function GetAssignedMarginSetName() As String
        Select Case currentMarginSlot
            Case "A"
                Return marginSetAName
            Case "B"
                Return marginSetBName
            Case Else
                Return Nothing
        End Select
    End Function

    ' Expose a list of all available sets (delegates to margin manager)
    Public Function ListAvailableMarginSets() As List(Of String)
        Return marginMgr.GetAllMarginSetNames()
    End Function





End Class

' GridFontSegmenter lays out maximal 5x7 cells (plus 1 cell gutter around and between) within a SpatialZone.
' Each segment is a 5x7 cell; use AllSegments to enumerate, GetSegment for bounds, and GetSurroundingStrips for gutter cells.
Public Class GridFontSegmenter
    Public Structure Segment
        Public IndexRow As Integer
        Public IndexCol As Integer
        Public StartRow As Integer
        Public StartCol As Integer
        Public EndRow As Integer
        Public EndCol As Integer
    End Structure

    Private ReadOnly zone As SpatialZone
    Private ReadOnly innerLeft As Integer
    Private ReadOnly innerRight As Integer
    Private ReadOnly innerTop As Integer
    Private ReadOnly innerBottom As Integer
    Private ReadOnly segmentColsCount As Integer
    Private ReadOnly segmentRowsCount As Integer
    Private ReadOnly nCols As Integer
    Private ReadOnly nRows As Integer

    ' Each segment is 5 wide, 7 tall, and surrounded by a 1-cell gutter (gap/strip)
    Public Const SegmentWidth As Integer = 5
    Public Const SegmentHeight As Integer = 7
    Public Const Strip As Integer = 1

    ' Create a font grid for the given SpatialZone
    Public Sub New(sz As SpatialZone, Optional fullscreenMode As Boolean = False)
        zone = sz
        If fullscreenMode Then
            innerLeft = sz.LeftColumn
            innerRight = sz.RightColumn
            innerTop = sz.TopRow
            innerBottom = sz.BottomRow
        Else
            innerLeft = sz.LeftColumn + 1
            innerRight = sz.RightColumn - 1
            innerTop = sz.TopRow + 1
            innerBottom = sz.BottomRow - 1
        End If

        segmentColsCount = ComputeSegmentCount(innerLeft, innerRight, SegmentWidth)
        segmentRowsCount = ComputeSegmentCount(innerTop, innerBottom, SegmentHeight)
        nCols = (Strip) + segmentColsCount * (SegmentWidth + Strip)
        nRows = (Strip) + segmentRowsCount * (SegmentHeight + Strip)
    End Sub

    ' Compute how many segments fit with border gutter and gutter between
    Private Function ComputeSegmentCount(side1 As Integer, side2 As Integer, segLen As Integer) As Integer
        ' Calculate the number of usable cells between side1 and side2,
        ' including both endpoints (e.g., if side1=2 and side2=5, length=4: columns 2,3,4,5).
        Dim length As Integer = side2 - side1 + 1

        ' Subtract the width of a gutter (Strip) from the total length,
        ' then divide by (segment width + gutter width) to get the number of full segments that fit.
        ' This ensures gutters and segments are fully within the available area, not overlapping borders.
        Return Math.Floor((length - Strip) / (segLen + Strip))
    End Function


    ' Number of segments horizontally
    Public ReadOnly Property SegmentColumns As Integer
        Get
            Return segmentColsCount
        End Get
    End Property

    ' Number of segments vertically
    Public ReadOnly Property SegmentRows As Integer
        Get
            Return segmentRowsCount
        End Get
    End Property

    ' Get a segment's bounds by row and column index
    Public Function GetSegment(rowIndex As Integer, colIndex As Integer) As Segment
        If rowIndex < 0 Or rowIndex >= segmentRowsCount Then Throw New ArgumentOutOfRangeException(NameOf(rowIndex))
        If colIndex < 0 Or colIndex >= segmentColsCount Then Throw New ArgumentOutOfRangeException(NameOf(colIndex))
        Dim startRow As Integer = innerTop + Strip + rowIndex * (SegmentHeight + Strip)
        Dim startCol As Integer = innerLeft + Strip + colIndex * (SegmentWidth + Strip)
        Dim endRow As Integer = startRow + SegmentHeight - 1
        Dim endCol As Integer = startCol + SegmentWidth - 1
        Return New Segment With {
            .IndexRow = rowIndex,
            .IndexCol = colIndex,
            .StartRow = startRow,
            .StartCol = startCol,
            .EndRow = endRow,
            .EndCol = endCol
        }
    End Function

    ' Enumerate all segments, left-to-right then top-to-bottom
    Public Iterator Function AllSegments() As IEnumerable(Of Segment)
        For row = 0 To segmentRowsCount - 1
            For col = 0 To segmentColsCount - 1
                Yield GetSegment(row, col)
            Next
        Next
    End Function

    ' Given a segment, get the points of the 4 surrounding gutters/strips (above, below, left, right)
    Public Sub GetSurroundingStrips(seg As Segment, ByRef above As List(Of (Integer, Integer)), ByRef below As List(Of (Integer, Integer)), ByRef left As List(Of (Integer, Integer)), ByRef right As List(Of (Integer, Integer)))
        above = New List(Of (Integer, Integer))
        below = New List(Of (Integer, Integer))
        left = New List(Of (Integer, Integer))
        right = New List(Of (Integer, Integer))
        Dim aboveRow = seg.StartRow - 1
        For c = seg.StartCol - 1 To seg.EndCol + 1
            above.Add((aboveRow, c))
        Next
        Dim belowRow = seg.EndRow + 1
        For c = seg.StartCol - 1 To seg.EndCol + 1
            below.Add((belowRow, c))
        Next
        Dim leftCol = seg.StartCol - 1
        For r = seg.StartRow - 1 To seg.EndRow + 1
            left.Add((r, leftCol))
        Next
        Dim rightCol = seg.EndCol + 1
        For r = seg.StartRow - 1 To seg.EndRow + 1
            right.Add((r, rightCol))
        Next
    End Sub

    ' True if (r,c) is inside the region covered by the font grid segments (not including margins/gutters)
    Public Function IsPointInGrid(r As Integer, c As Integer) As Boolean
        Return r >= innerTop + 1 AndAlso r < innerTop + nRows AndAlso c >= innerLeft + 1 AndAlso c < innerLeft + nCols
    End Function

    ' Returns any leftover area at the bottom/right of the zone that is not covered by segments
    Public Function GetUnusedArea() As (startRow As Integer, startCol As Integer, endRow As Integer, endCol As Integer)
        Dim usedEndRow = innerTop + nRows - 1
        Dim usedEndCol = innerLeft + nCols - 1
        Return (usedEndRow + 1, usedEndCol + 1, innerBottom - 1, innerRight - 1)
    End Function







































End Class ' maybe have spatial zone after map and object, update this info in a spatial zone class variable for API reading

' Usage Example:
' Dim zone As New SpatialZone("Zone1", marginMgr)     ' Use margin IDs 1,2,3,4 in marginMgr
' For Each seg In zone.FontSegmenter.AllSegments()    ' Enumerate all 5x7 segments in the zone
'     ' seg.StartRow, seg.StartCol, seg.EndRow, seg.EndCol define the bounds of the segment
'     ' zone.FontSegmenter.GetSurroundingStrips(seg, ...) gives the gutter cells around it
' Nex