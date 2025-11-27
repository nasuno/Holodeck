Imports System.Collections.Concurrent
Imports Current.PluginApi ' Namespace from your DLL

Public Class Margins
    Public Enum DefaultMarginName
        TopRow
        BottomRow
        LeftColumn
        RightColumn
    End Enum

    Public Class Margin
        Public Property ID As String             ' Name
        Public Property Type As MarginType       ' Row or Column
        Public Property Locked As Boolean        ' Cant remove or move
        Public Property Panel As PanelType       ' Current panel
        Public Property Row As Integer?          ' Only used if Type = RowMargin
        Public Property Column As Integer?       ' Only used if Type = ColumnMargin
        Public Property StructureID As Integer   ' For toggling Visibility via structureDrawState
        Public Property ObjectIds As List(Of Integer)

        Public Sub New(marginId As String,
                   marginType As MarginType,
                   panel As PanelType,
                   row As Integer?,
                   column As Integer?,
                   locked As Boolean,
                   structureId As Integer)
            Me.ID = marginId
            Me.Type = marginType
            Me.Panel = panel
            Me.Row = row
            Me.Column = column
            Me.Locked = locked
            Me.StructureID = structureId

            Me.ObjectIds = New List(Of Integer)()
        End Sub
    End Class

    Public Class MarginManager


        '================


        ' Global set of margin sets, keyed by set name
        Private ReadOnly marginSets As New Dictionary(Of String, Dictionary(Of String, String))

        ' Create a named margin set with two row and two column margins
        Public Sub CreateMarginSet(setName As String, topRowMarginId As String, bottomRowMarginId As String, leftColumnMarginId As String, rightColumnMarginId As String)
            ' Overwrite any existing set with the new values
            marginSets(setName) = New Dictionary(Of String, String) From {
                {"TopRow", topRowMarginId},
                {"BottomRow", bottomRowMarginId},
                {"LeftColumn", leftColumnMarginId},
                {"RightColumn", rightColumnMarginId}}
        End Sub

        ' Get all available margin set names
        Public Function GetAllMarginSetNames() As List(Of String)
            Return marginSets.Keys.ToList()
        End Function

        ' Get the contents of a margin set by name
        Public Function GetMarginSet(setName As String) As Dictionary(Of String, String)
            If Not marginSets.ContainsKey(setName) Then Return Nothing
            Return New Dictionary(Of String, String)(marginSets(setName))
        End Function

        ' Optionally: Remove or overwrite margin sets as needed



        '================

        Private ReadOnly margins As New ConcurrentDictionary(Of String, Margin)

        Public Sub CreateAllDefaultMargins()
            ' For each PanelType in [Enum].GetValues(GetType(PanelType)):
            For Each panel As PanelType In [Enum].GetValues(GetType(PanelType))
                ' Retrieve boundary info from panelData
                Dim leftCol As Integer = panelData.PanelInfo(panel)("FurthestLeftColumn")
                Dim rightCol As Integer = panelData.PanelInfo(panel)("FurthestRightColumn")
                Dim topRow As Integer = panelData.PanelInfo(panel)("FurthestTopRow")
                Dim bottomRow As Integer = panelData.PanelInfo(panel)("FurthestBottomRow")

                ' Create first-row margin
                CreateMargin(
                marginId:=panel.ToString() & "-" & DefaultMarginName.TopRow.ToString(),
                marginType:=MarginType.RowMargin,
                panel:=panel,
                row:=topRow,
                column:=Nothing,
                locked:=True
            )

                ' Create last-row margin
                CreateMargin(
                marginId:=panel.ToString() & "-" & DefaultMarginName.BottomRow.ToString(),
                marginType:=MarginType.RowMargin,
                panel:=panel,
                row:=bottomRow,
                column:=Nothing,
                locked:=True
            )

                ' Create first-column margin
                CreateMargin(
                marginId:=panel.ToString() & "-" & DefaultMarginName.LeftColumn.ToString(),
                marginType:=MarginType.ColumnMargin,
                panel:=panel,
                row:=Nothing,
                column:=leftCol,
                locked:=True
            )

                ' Create last-column margin
                CreateMargin(
                marginId:=panel.ToString() & "-" & DefaultMarginName.RightColumn.ToString(),
                marginType:=MarginType.ColumnMargin,
                panel:=panel,
                row:=Nothing,
                column:=rightCol,
                locked:=True
            )
            Next
        End Sub

        Public Sub CreateMargin(marginId As String,
                        marginType As MarginType,
                        panel As PanelType,
                        row As Integer?,
                        column As Integer?,
                        locked As Boolean)
            If marginType = MarginType.RowMargin AndAlso (Not row.HasValue) Then
                Throw New ArgumentException("For a RowMargin, you must provide the row parameter.")
            End If
            If marginType = MarginType.ColumnMargin AndAlso (Not column.HasValue) Then
                Throw New ArgumentException("For a ColumnMargin, you must provide the column parameter.")
            End If

            Dim structId = GetNextStructureId()

            Dim m As New Margin(
        marginId,
        marginType,
        panel,
        row,
        column,
        locked,
        structId
    )

            ' Build the 3D line representation for this margin, collecting IDs
            BuildMargin3DLine(marginType, panel, row, column, structId, m)
            Module1.structureDrawState(structId) = False

            ' Insert into dictionary
            margins.TryAdd(marginId, m)

            ' By default, we want the margin not visible:
            Module1.structureDrawState.TryAdd(structId, False)
        End Sub

        Public Sub RemoveMargin(marginId As String)
            Dim marginObj As Margin = Nothing
            If margins.TryGetValue(marginId, marginObj) Then
                ' Locked margins cannot be removed as per the design requirements.
                If marginObj.Locked Then
                    Throw New InvalidOperationException("Cannot remove a locked margin.")
                End If

                ' Remove from manager
                margins.TryRemove(marginId, marginObj)

                ' Turn off its structure visibility ' CONFUSED by this. is it not removed now? what is holding this?
                If Module1.structureDrawState.ContainsKey(marginObj.StructureID) Then
                    Module1.structureDrawState(marginObj.StructureID) = False
                End If
            End If
        End Sub

        Public Sub SetMarginLock(marginId As String, lockState As Boolean)
            Dim marginObj As Margin = Nothing
            If margins.TryGetValue(marginId, marginObj) Then
                marginObj.Locked = lockState
            End If
        End Sub

        Public Sub ToggleMarginVisibility(marginId As String)
            Dim marginObj As Margin = Nothing
            If margins.TryGetValue(marginId, marginObj) Then
                Dim currentVisible As Boolean = True
                Module1.structureDrawState.TryGetValue(marginObj.StructureID, currentVisible)
                Module1.structureDrawState(marginObj.StructureID) = Not currentVisible
            End If
        End Sub

        '---------------------------------------------------------------------------------
        ' Relocation methods:
        '   - PlusOne(marginId)
        '   - MinusOne(marginId)
        '   - Jump(marginId, newPanel, newRow/Column)
        '
        ' Wrapping logic:
        '   If row or column goes out of the range for that panel, we move to the adjacent
        '   panel. For row margin, we use "AdjacentAbove"/"AdjacentBelow". For column margin,
        '   we use "AdjacentLeft"/"AdjacentRight".
        '---------------------------------------------------------------------------------
        ' PlusOne: Move margin by +1 (row or column)
        Public Sub MarginPlusOne(marginId As String)
            Dim marginObj As Margin = Nothing
            If Not margins.TryGetValue(marginId, marginObj) Then Return
            If marginObj.Locked Then Throw New InvalidOperationException("Margin is locked.")
            If marginObj.Type = MarginType.RowMargin Then
                RelocateRowMargin(marginObj, +1)
            Else
                RelocateColumnMargin(marginObj, +1)
            End If
        End Sub


        ' MinusOne: Move margin by -1 (row or column)
        Public Sub MarginMinusOne(marginId As String)
            Dim marginObj As Margin = Nothing
            If Not margins.TryGetValue(marginId, marginObj) Then Return
            If marginObj.Locked Then Throw New InvalidOperationException("Margin is locked.")
            If marginObj.Type = MarginType.RowMargin Then
                RelocateRowMargin(marginObj, -1)
            Else
                RelocateColumnMargin(marginObj, -1)
            End If
        End Sub

        ' Jump to a new panel + row if row margin, or panel + column if column margin
        ' Jump: Move margin to a specific panel/row/column
        Public Sub MarginJump(marginId As String, newPanel As PanelType, newRow As Integer?, newCol As Integer?)
            Dim marginObj As Margin = Nothing
            If Not margins.TryGetValue(marginId, marginObj) Then Return
            If marginObj.Locked Then Throw New InvalidOperationException("Margin is locked.")

            marginObj.Panel = newPanel
            If marginObj.Type = MarginType.RowMargin Then
                If Not newRow.HasValue Then Throw New ArgumentException("A row margin must specify newRow in Jump.")
                marginObj.Row = newRow.Value
            Else
                If Not newCol.HasValue Then Throw New ArgumentException("A column margin must specify newCol in Jump.")
                marginObj.Column = newCol.Value
            End If

            RebuildMargin3DLine(marginObj)
        End Sub

        '---------------------------------------------------------------------------------
        ' Retrieve all margin IDs (plural) as a dictionary with two keys: "row" and "column",
        ' each key mapping to a list of margin IDs. This is kept thread-safe by enumerating
        ' the concurrent dictionary carefully.
        '---------------------------------------------------------------------------------
        Public Function GetAllMarginIDs() As Dictionary(Of String, List(Of String))
            Dim result As New Dictionary(Of String, List(Of String)) From {
            {"row", New List(Of String)()},
            {"column", New List(Of String)()}
        }
            For Each kvp In margins
                Dim mg As Margin = kvp.Value
                If mg.Type = MarginType.RowMargin Then
                    result("row").Add(mg.ID)
                Else
                    result("column").Add(mg.ID)
                End If
            Next
            Return result
        End Function

        '---------------------------------------------------------------------------------
        ' MarginInfo by ID (singular). We return:
        '   - isRowMargin or isColumnMargin
        '   - current position (panel, row or column)
        '   - locked state
        '   - name/ID
        '---------------------------------------------------------------------------------
        Public Function GetMarginInfoToString(marginId As String) As String
            Dim marginObj As Margin = Nothing
            If Not margins.TryGetValue(marginId, marginObj) Then
                Return $"No margin with ID {marginId} found."
            End If
            Dim sb As New Text.StringBuilder()
            sb.AppendLine($"Margin ID: {marginObj.ID}")
            sb.AppendLine($"Type: {marginObj.Type.ToString()}")
            sb.AppendLine($"Panel: {marginObj.Panel.ToString()}")
            sb.AppendLine($"Locked: {marginObj.Locked}")
            If marginObj.Type = MarginType.RowMargin Then
                sb.AppendLine($"Row: {marginObj.Row.Value}")
                sb.AppendLine("Column: (none for row margin)")
            Else
                sb.AppendLine("Row: (none for column margin)")
                sb.AppendLine($"Column: {marginObj.Column.Value}")
            End If
            Return sb.ToString()
        End Function

        '---------------------------------------------------------------------------------
        ' MarginInfo by ID (singular). Returns a dictionary: keys are property names,
        ' values are the property values in their original datatypes.
        '---------------------------------------------------------------------------------
        Public Function GetMarginInfo(marginId As String) As Dictionary(Of String, Object)
            Dim marginObj As Margin = Nothing
            If Not margins.TryGetValue(marginId, marginObj) Then
                Return Nothing
            End If

            Dim info As New Dictionary(Of String, Object) From {
        {"ID", marginObj.ID},
        {"Type", marginObj.Type},
        {"Panel", marginObj.Panel},
        {"Locked", marginObj.Locked},
        {"StructureID", marginObj.StructureID}
    }

            ' Row and Column are nullable, only include if present
            info.Add("Row", marginObj.Row)
            info.Add("Column", marginObj.Column)

            ' Optionally: remove Row or Column if they are Nothing
            ' If you prefer not to include nulls, uncomment below:
            'If marginObj.Row Is Nothing Then info.Remove("Row")
            'If marginObj.Column Is Nothing Then info.Remove("Column")

            Return info
        End Function

        '---------------------------------------------------------------------------------
        ' Internals
        '---------------------------------------------------------------------------------

        ' Build the 3D geometry for a margin by enumerating the relevant row or column
        ' indices for the specified panel in panelData, then calling AddMyObjectToFactory
        Private Sub BuildMargin3DLine(marginType As MarginType,
                              panel As PanelType,
                              row As Integer?,
                              column As Integer?,
                              structId As Integer,
                              mg As Margin) ' <-- added parameter
            Dim leftCol As Integer = panelData.PanelInfo(panel)("FurthestLeftColumn")
            Dim rightCol As Integer = panelData.PanelInfo(panel)("FurthestRightColumn")
            Dim topRow As Integer = panelData.PanelInfo(panel)("FurthestTopRow")
            Dim bottomRow As Integer = panelData.PanelInfo(panel)("FurthestBottomRow")

            ' Remove all objects from the global object dictionary before clearing the IDs list
            For Each id In mg.ObjectIds
                Module1.objectDictionary.TryRemove(id, Nothing)
            Next
            mg.ObjectIds.Clear() ' <-- clear existing IDs

            If marginType = MarginType.RowMargin Then
                Dim r = row.Value
                For c = leftCol To rightCol
                    Dim coord = panelDataGridLookup(panel, r, c)
                    'Dim objId = Module1.AddMyObjectToFactory(coord.Item1, coord.Item2, coord.Item3, structId) ' <-- capture ID
                    mg.ObjectIds.Add(objId) ' <-- store in Margin
                Next
            Else
                Dim c = column.Value
                For r = topRow To bottomRow
                    Dim coord = panelDataGridLookup(panel, r, c)
                    'Dim objId = Module1.AddMyObjectToFactory(coord.Item1, coord.Item2, coord.Item3, structId) ' <-- capture ID
                    mg.ObjectIds.Add(objId) ' <-- store in Margin
                Next
            End If
        End Sub

        ' Rebuild the 3D geometry for a margin after relocating, by:
        '   (1) turning off existing structure if present
        '   (2) building a new set of points
        Private Sub RebuildMargin3DLine(mg As Margin)
            ' Turn off old structure:
            If Module1.structureDrawState.ContainsKey(mg.StructureID) Then
                Module1.structureDrawState(mg.StructureID) = False
            End If

            ' Create a fresh structure ID and re-draw
            mg.StructureID = GetNextStructureId()
            BuildMargin3DLine(mg.Type, mg.Panel, mg.Row, mg.Column, mg.StructureID, mg)
            Module1.structureDrawState(mg.StructureID) = False

            ' By default, keep it invisible (unless toggled elsewhere)
            Module1.structureDrawState.TryAdd(mg.StructureID, False)
        End Sub


        ' For convenience, retrieve the panelData grid dictionary and return the (x,y,z)
        Private Function panelDataGridLookup(panel As PanelType, row As Integer, col As Integer) As (Integer, Integer, Integer)
            Dim grid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer)) =
        CType(panelData.PanelInfo(panel)("Grid"), Dictionary(Of (Integer, Integer), (Integer, Integer, Integer)))

            Dim key = (row, col)
            If grid.ContainsKey(key) Then
                Return grid(key)
            ElseIf grid.Count > 0 Then
                ' Clamp to nearest valid key (Manhattan distance)
                Dim validKey = grid.Keys.OrderBy(Function(k) Math.Abs(k.Item1 - row) + Math.Abs(k.Item2 - col)).First()
                Return grid(validKey)
            Else
                Throw New KeyNotFoundException($"No valid keys in grid for panel {panel}")
            End If
        End Function


        ' For margins of type RowMargin, shift row up or down. If out of range, we wrap to
        ' an adjacent panel in which the row remains in range (top/bottom).
        ' Relocate a row margin up or down and rebuild its 3D line
        Private Sub RelocateRowMargin(mg As Margin, delta As Integer)
            If mg.Locked Then Exit Sub
            Dim currentRow As Integer = mg.Row.Value
            Dim topRow As Integer = panelData.PanelInfo(mg.Panel)("FurthestTopRow")
            Dim bottomRow As Integer = panelData.PanelInfo(mg.Panel)("FurthestBottomRow")
            currentRow += delta

            If currentRow < topRow Then
                ' Move to adjacent "above" panel
                Dim newPanel As PanelType = panelData.PanelInfo(mg.Panel)("AdjacentAbove")
                mg.Panel = newPanel
                mg.Row = panelData.PanelInfo(newPanel)("FurthestBottomRow")
            ElseIf currentRow > bottomRow Then
                ' Move to adjacent "below" panel
                Dim newPanel As PanelType = panelData.PanelInfo(mg.Panel)("AdjacentBelow")
                mg.Panel = newPanel
                mg.Row = panelData.PanelInfo(newPanel)("FurthestTopRow")
            Else
                mg.Row = currentRow
            End If

            RebuildMargin3DLine(mg)
        End Sub

        ' For margins of type ColumnMargin, shift column left or right. If out of range,
        ' we wrap to an adjacent panel in which the column remains in range (left/right).
        ' RelocateColumnMargin: Moves a column margin left or right (helper, called by Plus/MinusOne)
        Private Sub RelocateColumnMargin(mg As Margin, delta As Integer)
            If mg.Locked Then Exit Sub
            Dim currentCol As Integer = mg.Column.Value
            Dim leftCol As Integer = panelData.PanelInfo(mg.Panel)("FurthestLeftColumn")
            Dim rightCol As Integer = panelData.PanelInfo(mg.Panel)("FurthestRightColumn")
            currentCol += delta

            If currentCol < leftCol Then
                ' Move to adjacent "left" panel
                Dim newPanel As PanelType = panelData.PanelInfo(mg.Panel)("AdjacentLeft")
                mg.Panel = newPanel
                mg.Column = panelData.PanelInfo(newPanel)("FurthestRightColumn")
            ElseIf currentCol > rightCol Then
                ' Move to adjacent "right" panel
                Dim newPanel As PanelType = panelData.PanelInfo(mg.Panel)("AdjacentRight")
                mg.Panel = newPanel
                mg.Column = panelData.PanelInfo(newPanel)("FurthestLeftColumn")
            Else
                mg.Column = currentCol
            End If

            RebuildMargin3DLine(mg)
        End Sub

        '---------------------------------------------------------------------------------
        ' Utility to get a unique structure ID for new margin geometry
        '---------------------------------------------------------------------------------
        Private Function GetNextStructureId() As Integer
            Return Module1.GetNextUniqId()
        End Function

    End Class

    '=================================================================================
    ' NOTES:
    ' 1) The user (programmer) can do something like:
    '
    '    Dim marginMgr As New MarginManager(panelData)
    '    marginMgr.CreateAllDefaultMargins()  ' sets up locked default margins
    '
    '    marginMgr.CreateMargin("MyRowMargin1", MarginType.RowMargin, PanelType.TopPanel, 10, Nothing, locked:=False)
    '    marginMgr.MarginPlusOne("MyRowMargin1")     ' Moves row margin down 1 or wraps
    '    marginMgr.ToggleMarginVisibility("MyRowMargin1") ' toggles display
    '
    ' 2) All adjacency logic depends on correct "AdjacentAbove"/"AdjacentBelow"/"AdjacentLeft"/
    '    "AdjacentRight" assignments within PanelDataManager.PanelInfo(...).
    '
    ' 3) The concurrency for listing margin IDs uses a separate dictionary approach,
    '    enumerating margins safely from the concurrent dictionary. 
    '
    ' 4) This code sample presumes that PanelDataManager grids hold integer keys for
    '    (Row,Column) with the corresponding cartesian coordinates in the dictionary values.
    '=================================================================================



    '=================================================================================
    ' USAGE GUIDE FOR MarginManager
    '=================================================================================

    ' 1) TOGGLE VISIBILITY OF A MARGIN
    ' You can toggle the visibility of a margin (e.g., "NorthPanel-FirstColumn") using:
    ' marginMgr.ToggleMarginVisibility("NorthPanel-FirstColumn")

    ' 2) CREATE A NEW MARGIN
    ' To create a new column margin on NorthPanel at column 5 with a custom ID ("testmargin"):
    ' marginMgr.CreateMargin("testmargin", MarginType.ColumnMargin, PanelType.NorthPanel, Nothing, 5, locked:=False)

    ' 3) MOVE A MARGIN DYNAMICALLY
    ' Use the following methods to relocate margins:
    ' a) Increment (plus one step):
    '    marginMgr.MarginPlusOne("testmargin")
    ' b) Decrement (minus one step):
    '    marginMgr.MarginMinusOne("testmargin")
    ' c) Jump to a specific location:
    '    marginMgr.MarginJump("testmargin", PanelType.NorthPanel, Nothing, 7) ' Moves to column 7

    ' 4) LOCK OR UNLOCK A MARGIN
    ' Locking a margin prevents it from being moved or removed:
    ' marginMgr.SetMarginLock("testmargin", lockState:=True)
    ' Unlocking allows movement or removal:
    ' marginMgr.SetMarginLock("testmargin", lockState:=False)

    ' 5) REMOVE A MARGIN
    ' If the margin is unlocked, you can remove it using:
    ' marginMgr.RemoveMargin("testmargin")
    ' Note: Locked margins cannot be removed. Unlock them first if necessary.

    ' 6) DEFAULT MARGINS
    ' Default margins ("FirstRow", "LastRow", "FirstColumn", "LastColumn") are created
    ' for all panels automatically by calling:
    ' marginMgr.CreateAllDefaultMargins()
    ' These margins are locked by default. Unlock them to modify or remove.

    ' 7) HOW CAN MULTIPLE MARGINS HAVE THE SAME NAME?
    ' Margins are uniquely identified by their "ID" in the MarginManager. For example:
    ' - "NorthPanel-FirstColumn" and "SouthPanel-FirstColumn" are unique because their
    '   IDs include the panel name, even though "FirstColumn" is the same.
    ' - Custom margins like "testmargin" are differentiated by their unique IDs.
    ' The MarginManager ensures IDs are unique even if the "name" part is reused.

    ' 8) ADVANCED USAGE
    ' a) To get all margin IDs grouped by type (row or column):
    '    Dim marginIDs = marginMgr.GetAllMarginIDs()
    '    Console.WriteLine("Row Margins: " & String.Join(", ", marginIDs("row")))
    '    Console.WriteLine("Column Margins: " & String.Join(", ", marginIDs("column")))
    ' b) To retrieve detailed information about a specific margin:
    '    Dim info = marginMgr.GetMarginInfo("testmargin")
    '    Console.WriteLine(info)

    ' NOTES:
    ' - Default adjacency rules (e.g., "AdjacentAbove", "AdjacentBelow") must be configured
    '   correctly in the PanelDataManager for relocation to work properly.
    ' - The CreateMargin method automatically generates 3D representations for margins
    '   using the BuildMargin3DLine method.
    '=================================================================================

End Class