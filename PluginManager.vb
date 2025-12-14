' =======================================================
' ===============  API IMPLEMENTATION  ==================
' =======================================================

Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.IO
Imports System.Reflection
Imports System.Threading
Imports Current.PluginApi

' =======================================================
' ===============  API IMPLEMENTATION CLASS  ============
' =======================================================

Public Class CurrentApiImpl
    Implements ICurrentApi

    ' === OBJECT CONTROL ===
    Public Sub RemoveObjectsByStructureId(structureId As Integer) Implements ICurrentApi.RemoveObjectsByStructureId
        Module1.RemoveObjectsByStructureId(structureId)
    End Sub

    Public Function GetNextUniqId() As Integer Implements ICurrentApi.GetNextUniqId
        Return Module1.GetNextUniqId()
    End Function

    Public Function GetMyObjectByStructureId(structureId As Integer) As MyObject Implements ICurrentApi.GetMyObjectByStructureId
        Return Module1.GetMyObjectByStructureId(structureId)
    End Function

    Public ReadOnly Property objectDictionary As ConcurrentDictionary(Of Integer, MyObject) Implements ICurrentApi.objectDictionary
        Get
            Return Module1.objectDictionary
        End Get
    End Property

    Public ReadOnly Property structureObjectIDs As ConcurrentDictionary(Of Integer, ImmutableList(Of Integer)) Implements ICurrentApi.structureObjectIDs
        Get
            Return Module1.structureObjectIDs
        End Get
    End Property

    Public Sub SetStructureDrawState(structureId As Integer, isOn As Boolean) Implements ICurrentApi.SetStructureDrawState
        Module1.structureDrawState(structureId) = isOn
    End Sub

    ' === UTILITY ===
    Public Function Bresenham3D(startX As Integer, startY As Integer, startZ As Integer,
                                endX As Integer, endY As Integer, endZ As Integer) As List(Of (Integer, Integer, Integer)) Implements ICurrentApi.Bresenham3D
        Return Module1.Bresenham3D(startX, startY, startZ, endX, endY, endZ)
    End Function

    Public Sub ThinEvenSpatiallyAdaptiveAuto(
        ByRef sourceDict As ConcurrentDictionary(Of Integer, MyObject),
        ByRef destDict As ConcurrentDictionary(Of Integer, MyObject),
        numToLeave As Integer,
        observer As (Integer, Integer, Integer),
        keepRadius As Double,
        Optional numBands As Integer = 10,
        Optional closeBiasExponent As Double = 1.5
    ) Implements ICurrentApi.ThinEvenSpatiallyAdaptiveAuto
        Module1.ThinEvenSpatiallyAdaptiveAuto(sourceDict, destDict, numToLeave, observer, keepRadius, numBands, closeBiasExponent)
    End Sub

    ' === TRIANGLE CONTROL ===
    Public Sub RemoveAllTrianglesInSet(setId As Integer) Implements ICurrentApi.RemoveAllTrianglesInSet
        Module1.RemoveAllTrianglesInSet(setId)
    End Sub

    Public Function AddTriangle(x1 As Double, y1 As Double, z1 As Double,
                               x2 As Double, y2 As Double, z2 As Double,
                               x3 As Double, y3 As Double, z3 As Double,
                               setId As Integer) As Integer Implements ICurrentApi.AddTriangle
        Return Module1.AddTriangle(x1, y1, z1, x2, y2, z2, x3, y3, z3, setId)
    End Function

    Public ReadOnly Property triangleGroups As Object Implements ICurrentApi.triangleGroups
        Get
            Return Module1.triangleGroups
        End Get
    End Property

    Public ReadOnly Property trianglesById As Object Implements ICurrentApi.trianglesById
        Get
            Return Module1.trianglesById
        End Get
    End Property

    ' ===================================================
    ' ===============  PANEL BOUNDS  ====================
    ' ===================================================

    Public Function GetPanelFurthestLeftColumn(panel As PanelType) As Integer Implements ICurrentApi.GetPanelFurthestLeftColumn
        Return panelData.PanelInfo(panel)("FurthestLeftColumn")
    End Function

    Public Function GetPanelFurthestTopRow(panel As PanelType) As Integer Implements ICurrentApi.GetPanelFurthestTopRow
        Return panelData.PanelInfo(panel)("FurthestTopRow")
    End Function

    Public Function GetPanelFurthestRightColumn(panel As PanelType) As Integer Implements ICurrentApi.GetPanelFurthestRightColumn
        Return panelData.PanelInfo(panel)("FurthestRightColumn")
    End Function

    Public Function GetPanelFurthestBottomRow(panel As PanelType) As Integer Implements ICurrentApi.GetPanelFurthestBottomRow
        Return panelData.PanelInfo(panel)("FurthestBottomRow")
    End Function

    ' ===================================================
    ' ===============  MARGIN/ZONE CONTROL  =============
    ' ===================================================

    Private marginMgr As Margins.MarginManager

    Public Sub New(marginMgr As Margins.MarginManager)
        Me.marginMgr = marginMgr
    End Sub

    Public Sub MarginJump(marginId As String, newPanel As PanelType, newRow As Integer?, newCol As Integer?) Implements ICurrentApi.MarginJump
        marginMgr.MarginJump(marginId, newPanel, newRow, newCol)
    End Sub

    Public Sub MarginPlusOne(marginId As String) Implements ICurrentApi.MarginPlusOne
        marginMgr.MarginPlusOne(marginId)
    End Sub

    Public Sub MarginMinusOne(marginId As String) Implements ICurrentApi.MarginMinusOne
        marginMgr.MarginMinusOne(marginId)
    End Sub

    Public Sub SetMarginLock(marginId As String, lockState As Boolean) Implements ICurrentApi.SetMarginLock
        marginMgr.SetMarginLock(marginId, lockState)
    End Sub

    Public Sub RemoveMargin(marginId As String) Implements ICurrentApi.RemoveMargin
        marginMgr.RemoveMargin(marginId)
    End Sub

    Public Sub ToggleMarginVisibility(marginId As String) Implements ICurrentApi.ToggleMarginVisibility
        marginMgr.ToggleMarginVisibility(marginId)
    End Sub








    Public Function AddMyObjectToFactory(x As Integer, y As Integer, z As Integer, structureId As Integer) As Integer Implements ICurrentApi.AddMyObjectToFactory
        Return Module1.AddMyObjectToFactory(x, y, z, structureId)
    End Function

    Public Sub CreateMargin(marginId As String, marginType As MarginType, panel As PanelType, row As Integer?, column As Integer?, locked As Boolean) Implements ICurrentApi.CreateMargin
        marginMgr.CreateMargin(marginId, marginType, panel, row, column, locked)
    End Sub

    Public Function CreateSpatialZone(zoneId As String) As ISpatialZone Implements ICurrentApi.CreateSpatialZone
        If Not spatialZoneDict.ContainsKey(zoneId) Then
            Dim zone As New SpatialZone(zoneId, marginMgr)
            spatialZoneDict.Add(zoneId, zone)
        Else
            Console.WriteLine($"SpatialZone with ID '{zoneId}' already exists.")
        End If
        Return New SpatialZoneAdapter(spatialZoneDict(zoneId))
    End Function

    Public Sub RemoveSpatialZone(zoneId As String) Implements ICurrentApi.RemoveSpatialZone
        '    If spatialZoneDict.ContainsKey(zoneId) Then
        '        spatialZoneDict(zoneId).DisposeZone()
        '        spatialZoneDict.Remove(zoneId)
        '        Console.WriteLine($"SpatialZone '{zoneId}' removed and disposed.")
        '    Else
        '        Console.WriteLine($"SpatialZone '{zoneId}' does not exist.")
        '    End If
    End Sub

    Public Function GetSpatialZone(zoneId As String) As ISpatialZone Implements ICurrentApi.GetSpatialZone
        If spatialZoneDict.ContainsKey(zoneId) Then
            Return New SpatialZoneAdapter(spatialZoneDict(zoneId))
        Else
            Return Nothing
        End If
    End Function

    Public Function GetAllMarginIDs() As Dictionary(Of String, List(Of String)) Implements ICurrentApi.GetAllMarginIDs
        Return marginMgr.GetAllMarginIDs()
    End Function

    Public Function GetMarginInfoSnapshot(marginId As String) As Dictionary(Of String, Object) Implements ICurrentApi.GetMarginInfoSnapshot
        Return marginMgr.GetMarginInfo(marginId)
    End Function


    Public Sub CreateMarginSet(setName As String, topRowMarginId As String, bottomRowMarginId As String, leftColumnMarginId As String, rightColumnMarginId As String) Implements ICurrentApi.CreateMarginSet
        marginMgr.CreateMarginSet(setName, topRowMarginId, bottomRowMarginId, leftColumnMarginId, rightColumnMarginId)
    End Sub

    Public Function GetAllMarginSetNames() As List(Of String) Implements ICurrentApi.GetAllMarginSetNames
        Return marginMgr.GetAllMarginSetNames()
    End Function

    Public Function GetMarginSet(setName As String) As Dictionary(Of String, String) Implements ICurrentApi.GetMarginSet
        Return marginMgr.GetMarginSet(setName)
    End Function


    ' Assign a margin set name into slot A for the given zone.
    Public Sub AssignZoneMarginSetA(zoneId As String, setName As String) _
    Implements ICurrentApi.AssignZoneMarginSetA

        If spatialZoneDict.ContainsKey(zoneId) Then
            spatialZoneDict(zoneId).AssignMarginSetA(setName)
        End If
    End Sub

    ' Assign a margin set name into slot B for the given zone.
    Public Sub AssignZoneMarginSetB(zoneId As String, setName As String) _
    Implements ICurrentApi.AssignZoneMarginSetB

        If spatialZoneDict.ContainsKey(zoneId) Then
            spatialZoneDict(zoneId).AssignMarginSetB(setName)
        End If
    End Sub

    ' Switch the zone to whatever is in slot A (throws if unset at zone level).
    Public Sub SwitchZoneToMarginSetA(zoneId As String) _
    Implements ICurrentApi.SwitchZoneToMarginSetA

        If spatialZoneDict.ContainsKey(zoneId) Then
            spatialZoneDict(zoneId).SwitchToMarginSetA()
        End If
    End Sub

    ' Switch the zone to whatever is in slot B (throws if unset at zone level).
    Public Sub SwitchZoneToMarginSetB(zoneId As String) _
    Implements ICurrentApi.SwitchZoneToMarginSetB

        If spatialZoneDict.ContainsKey(zoneId) Then
            spatialZoneDict(zoneId).SwitchToMarginSetB()
        End If
    End Sub

    ' Toggle between A and B in the zone, using the zone’s SwapToAlternateMarginSet().
    Public Sub SwapZoneMarginSets(zoneId As String) _
    Implements ICurrentApi.SwapZoneMarginSets

        If spatialZoneDict.ContainsKey(zoneId) Then
            spatialZoneDict(zoneId).SwapToAlternateMarginSet()
        End If
    End Sub

    ' Returns the margin set name currently active in this zone (A or B).
    Public Function GetZoneAssignedMarginSet(zoneId As String) As String _
    Implements ICurrentApi.GetZoneAssignedMarginSet

        If spatialZoneDict.ContainsKey(zoneId) Then
            Return spatialZoneDict(zoneId).GetAssignedMarginSetName()
        End If
        Return Nothing
    End Function



    ' ===================================================
    ' ===============  SPATIAL ZONE ENUMERATION  =========
    ' ===================================================

    Public Function GetAllSpatialZones() As IEnumerable(Of ISpatialZone) _
        Implements ICurrentApi.GetAllSpatialZones

        SyncLock spatialZoneDict
            Return spatialZoneDict.Values.
                   Select(Function(z) New SpatialZoneAdapter(z)).ToList()
        End SyncLock
    End Function

    ' ===================================================
    ' ===============  OBSERVER VECTOR DATA  =============
    ' ===================================================

    Public Function GetObserverOrigin() As (Integer, Integer, Integer) _
        Implements ICurrentApi.GetObserverOrigin

        ' Program.observerVectorData("FromPoint") was set as a ValueTuple(Of Int,Int,Int)
        Return CType(observerVectorData("FromPoint"), ValueTuple(Of Integer, Integer, Integer))
    End Function

    Public Function GetObserverUnitVector() As (Double, Double, Double) _
        Implements ICurrentApi.GetObserverUnitVector

        ' Program.observerVectorData("UnitVector") is a Vector3D host‐type with ToTuple()
        Dim vecObj = observerVectorData("UnitVector")
        If vecObj Is Nothing Then
            Return (0.0, 0.0, 0.0)
        End If
        Return CType(vecObj.GetType() _
              .GetMethod("ToTuple") _
              .Invoke(vecObj, Nothing),
              ValueTuple(Of Double, Double, Double))
    End Function

End Class

' =======================================================
' ===============  SPATIAL ZONE ADAPTER  ================
' =======================================================

' Adapts a SpatialZone for plugin- API access (exposes only ISpatialZone methods/properties)
Public Class SpatialZoneAdapter
    Implements ISpatialZone

    Private ReadOnly _zone As SpatialZone ' The wrapped internal SpatialZone

    Public Sub New(realZone As SpatialZone)
        _zone = realZone ' Constructor: wraps a SpatialZone instance
    End Sub

    Public Sub UpdateMargins(leftId As String, rightId As String, topId As String, bottomId As String) Implements ISpatialZone.UpdateMargins
        _zone.UpdateMargins(leftId, rightId, topId, bottomId) ' Update the margins on the zone
    End Sub

    Public Sub DisposeZone() Implements ISpatialZone.DisposeZone
        '_zone.DisposeZone() ' Clean up all objects/resources created by this zone
    End Sub

    Public ReadOnly Property Left As Integer Implements ISpatialZone.Left
        Get
            Return _zone.LeftColumn ' The left boundary (column index) of the zone on its panel
        End Get
    End Property
    Public ReadOnly Property Right As Integer Implements ISpatialZone.Right
        Get
            Return _zone.RightColumn ' The right boundary (column index) of the zone on its panel
        End Get
    End Property
    Public ReadOnly Property Top As Integer Implements ISpatialZone.Top
        Get
            Return _zone.TopRow ' The top boundary (row index) of the zone on its panel
        End Get
    End Property
    Public ReadOnly Property Bottom As Integer Implements ISpatialZone.Bottom
        Get
            Return _zone.BottomRow ' The bottom boundary (row index) of the zone on its panel
        End Get
    End Property

    Public Property Text As String Implements ISpatialZone.Text
        Get
            Return _zone.Text
        End Get
        Set(value As String)
            _zone.Text = value
        End Set
    End Property

    Public ReadOnly Property ID As String Implements ISpatialZone.ID
        Get
            Return _zone.ID
        End Get
    End Property

    Public Function GetAllFontSegments() As List(Of (Integer, Integer)) Implements ISpatialZone.GetAllFontSegments
        Dim segs As New List(Of (Integer, Integer))
        If _zone.FontSegmenter IsNot Nothing Then
            For Each seg In _zone.FontSegmenter.AllSegments()
                segs.Add((seg.IndexRow, seg.IndexCol))
            Next
        End If
        Return segs
    End Function

    Public Sub SetGutterVisible(row As Integer, col As Integer, side As String, visible As Boolean) Implements ISpatialZone.SetGutterVisible
        _zone.SetGutterVisible(row, col, side, visible)
    End Sub

    Public ReadOnly Property WrappedCharIndex As Dictionary(Of (Integer, Integer), Char) _
        Implements ISpatialZone.WrappedCharIndex
        Get
            Return _zone.WrappedCharIndex
        End Get
    End Property

    Public ReadOnly Property BoundingBoxAABB As ((Integer, Integer, Integer), (Integer, Integer, Integer)) _
        Implements ISpatialZone.BoundingBoxAABB
        Get
            Return _zone.BoundingBoxAABB
        End Get
    End Property

    ' ──────────────── NEW ──────────────── *** DO NOT USE ***
    Public Sub InvertColorsOn() Implements ISpatialZone.InvertColorsOn
        _zone.InvertColorsOn()
    End Sub

    Public Sub InvertColorsOff() Implements ISpatialZone.InvertColorsOff
        _zone.InvertColorsOff()
    End Sub
    ' ─────────────────────────────────────

    ' Add more methods as needed
End Class

' =======================================================
' ===============  PLUGIN MANAGER  ======================
' =======================================================

Public Class PluginManager
    Public Class PluginInfo
        Public Property PluginInstance As IPlugin
        Public Property PluginMetadata As PluginMetadataAttribute
    End Class

    Public Property Plugins As List(Of PluginInfo) = New List(Of PluginInfo)()
    Private apiRef As ICurrentApi
    Private pluginThreads As List(Of Thread) = New List(Of Thread)()

    ' Loads plugins from the plugins directory
    Public Sub LoadPlugins()
        Dim pluginDir As String = IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "plugins")
        If Not IO.Directory.Exists(pluginDir) Then IO.Directory.CreateDirectory(pluginDir)
        Dim pluginAssemblyPaths = IO.Directory.GetFiles(pluginDir, "*.dll")
        Dim pluginAssemblies = pluginAssemblyPaths.Select(AddressOf Assembly.LoadFrom)

        Plugins.Clear()

        For Each pluginAssembly As Assembly In pluginAssemblies
            For Each pluginType As Type In pluginAssembly.GetTypes()
                If GetType(IPlugin).IsAssignableFrom(pluginType) AndAlso Not pluginType.IsInterface AndAlso Not pluginType.IsAbstract Then
                    Dim pluginMetadata = TryCast(pluginType.GetCustomAttribute(GetType(PluginMetadataAttribute)), PluginMetadataAttribute)
                    If pluginMetadata IsNot Nothing Then
                        Dim pluginInstance = DirectCast(Activator.CreateInstance(pluginType), IPlugin)
                        Plugins.Add(New PluginInfo With {
                            .PluginInstance = pluginInstance,
                            .PluginMetadata = pluginMetadata
                        })
                    End If
                End If
            Next
        Next

        If Plugins.Count = 0 Then
            Console.WriteLine("No plugins found.")
        Else
            Console.WriteLine("Plugins loaded.")
            'Console.WriteLine("Available plugins:")
            'For Each plugin As PluginInfo In Plugins
            '    With plugin.PluginMetadata
            '        Console.WriteLine($"- '{ .Name}' v{ .Version} by { .Author}: { .Description}")
            '    End With
            'Next
        End If
    End Sub

    ' Runs all loaded plugins on their own threads
    Public Sub RunAllPluginsOnThreads(api As ICurrentApi)
        apiRef = api
        pluginThreads.Clear()
        If Plugins IsNot Nothing Then
            For Each plugin As PluginInfo In Plugins
                Dim thread As New Thread(AddressOf PluginThreadCallback)
                thread.IsBackground = True
                thread.Start(plugin)
                pluginThreads.Add(thread)
            Next
        End If
    End Sub

    ' Runs a single plugin by name on its own thread
    Public Sub RunPluginByNameOnThread(pluginName As String, api As ICurrentApi)
        apiRef = api ' Ensure the API reference is set for the plugin thread
        Dim plugin As PluginInfo = Plugins.FirstOrDefault(Function(p) p.PluginMetadata.Name = pluginName)
        If plugin Is Nothing Then
            Console.WriteLine($"Plugin '{pluginName}' not found.")
            Return
        End If
        Dim thread As New Thread(AddressOf PluginThreadCallback)
        thread.IsBackground = True
        thread.Start(plugin)
        pluginThreads.Add(thread)
    End Sub

    ' Thread callback to execute the plugin
    Private Sub PluginThreadCallback(state As Object)
        Dim plugin As PluginInfo = CType(state, PluginInfo)
        With plugin.PluginMetadata
            Console.WriteLine($"Loaded plugin: '{ .Name}' v{ .Version} by { .Author}: { .Description}")
        End With
        plugin.PluginInstance.Execute(apiRef)
    End Sub

    ' Reports available plugins to the console
    Public Sub ListAvailablePlugins()
        If Plugins.Count = 0 Then
            Console.WriteLine("No plugins loaded.")
        Else
            Console.WriteLine("Available plugins:")
            For i As Integer = 0 To Plugins.Count - 1
                Dim meta = Plugins(i).PluginMetadata
                Console.WriteLine($"{i + 1}: '{meta.Name}' v{meta.Version} by {meta.Author} - {meta.Description}")
            Next
        End If
    End Sub

End Class








Public Module IniManager
    ' ============================================================================
    ' HOW TO ADD A NEW INI SECTION: 
    ' 
    ' 1. Add your section's default content to GetDefaultIniContent()
    ' 2. If your section uses key=value pairs, use GetSectionInt() or GetSectionString()
    ' 3. If your section needs special parsing (like Commands), add a loader below
    '    and call it from InitializeSectionLoaders()
    ' ============================================================================

    Private ReadOnly SectionData As New Dictionary(Of String, List(Of String))(StringComparer.OrdinalIgnoreCase)
    Private IsInitialized As Boolean = False
    Private Const IniFileName As String = "commands.ini"
    Private Const PluginsFolderName As String = "plugins"

    ' ----------------------------------------------------------------------------
    ' SECTION-SPECIFIC STORAGE
    ' Add new dictionaries or variables here for sections that need special parsing
    ' ----------------------------------------------------------------------------
    Private ReadOnly CommandMap As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

    ' ----------------------------------------------------------------------------
    ' DEFAULT INI CONTENT
    ' Add new sections here - this defines what gets written when INI is created
    ' ----------------------------------------------------------------------------
    Private Function GetDefaultIniContent() As String()
        Return {
            "[Commands]",
            "; Command to plugin mappings",
            "; Two lines per mapping:",
            ";   1) command string as received by the host",
            ";   2) plugin name (must match PluginMetadata.Name)",
            "",
            "; Dev:  event subsystem & helpers",
            "events",
            "Event Aggregator",
            "",
            "; Dev: menu subsystem plugin",
            "menu system",
            "Menu System",
            "",
            "; Dev:  test of menu subsystem",
            "test menu",
            "Menu System Consumer",
            "",
            "; Built-in: list all plugins",
            "list plugins",
            "_builtin_list_plugins",
            "",
            "; Built-in: list margins",
            "list margins",
            "_builtin_list_margins",
            "",
            "run cube",
            "_builtin_run_cube",
            "",
            "[CubeCenter]",
            "; Cube center coordinates - EDIT THESE VALUES",
            "CenterX=-250",
            "CenterY=73",
            "CenterZ=-78",
            "",
            "[Tuning]",
            "; Adjustable runtime parameters",
            "RequiredStationaryFrames=5",
            "ScoreDecayPerFrame=1",
            "ScoreBumpOnBlock=8",
            "",
            "[predefined margin sets]",
            "; Your margin sets here",
            ""
        }
    End Function

    ' ----------------------------------------------------------------------------
    ' SECTION LOADERS
    ' Add new loader methods here for sections that need special parsing
    ' Then register them in InitializeSectionLoaders()
    ' ----------------------------------------------------------------------------
    Private Sub InitializeSectionLoaders()
        LoadCommandsSection()
        ' ADD NEW SECTION LOADERS HERE: 
        ' LoadMarginSetsSection()
        ' LoadWhateverSection()
    End Sub

    Private Sub LoadCommandsSection()
        Dim lines As List(Of String) = Nothing
        If Not SectionData.TryGetValue("Commands", lines) Then Return

        Dim pendingCommand As String = Nothing

        For Each raw In lines
            Dim line = raw.Trim()

            If IsCommentOrBlank(line) Then Continue For

            If pendingCommand Is Nothing Then
                pendingCommand = raw.TrimEnd(vbCr, vbLf)
            Else
                Dim target = raw.TrimEnd(vbCr, vbLf)
                If pendingCommand.Length > 0 Then
                    CommandMap(pendingCommand) = target
                End If
                pendingCommand = Nothing
            End If
        Next
    End Sub

    ' ----------------------------------------------------------------------------
    ' PUBLIC ACCESSORS
    ' Add new public functions here for accessing section data
    ' ----------------------------------------------------------------------------

    ' Use for any section with key=value integer pairs
    Public Function GetSectionInt(sectionName As String, key As String, defaultValue As Integer) As Integer
        EnsureInitialized()
        Dim lines As List(Of String) = Nothing
        If Not SectionData.TryGetValue(sectionName, lines) Then Return defaultValue

        Dim lowerKey = key.ToLower()
        For Each raw In lines
            Dim line = raw.Trim()
            If IsCommentOrBlank(line) Then Continue For

            Dim eqIndex = line.IndexOf("="c)
            If eqIndex > 0 Then
                Dim k = line.Substring(0, eqIndex).Trim().ToLower()
                Dim v = line.Substring(eqIndex + 1).Trim()
                If k = lowerKey Then
                    Dim result As Integer
                    If Integer.TryParse(v, result) Then Return result
                End If
            End If
        Next
        Return defaultValue
    End Function

    ' Use for any section with key=value string pairs
    Public Function GetSectionString(sectionName As String, key As String, defaultValue As String) As String
        EnsureInitialized()
        Dim lines As List(Of String) = Nothing
        If Not SectionData.TryGetValue(sectionName, lines) Then Return defaultValue

        Dim lowerKey = key.ToLower()
        For Each raw In lines
            Dim line = raw.Trim()
            If IsCommentOrBlank(line) Then Continue For

            Dim eqIndex = line.IndexOf("="c)
            If eqIndex > 0 Then
                Dim k = line.Substring(0, eqIndex).Trim().ToLower()
                Dim v = line.Substring(eqIndex + 1).Trim()
                If k = lowerKey Then Return v
            End If
        Next
        Return defaultValue
    End Function

    ' Use for Commands section lookups
    Public Function TryGetPluginForCommand(command As String, ByRef pluginName As String) As Boolean
        EnsureInitialized()
        If String.IsNullOrEmpty(command) Then
            pluginName = Nothing
            Return False
        End If
        Return CommandMap.TryGetValue(command, pluginName)
    End Function

    ' Use to get raw lines from any section (for custom parsing)
    Public Function GetSectionLines(sectionName As String) As List(Of String)
        EnsureInitialized()
        Dim lines As List(Of String) = Nothing
        If SectionData.TryGetValue(sectionName, lines) Then
            Return New List(Of String)(lines)
        End If
        Return New List(Of String)()
    End Function

    ' ----------------------------------------------------------------------------
    ' CORE INFRASTRUCTURE
    ' You probably don't need to modify anything below this line
    ' ----------------------------------------------------------------------------
    Private Function IsCommentOrBlank(line As String) As Boolean
        Return line = "" OrElse line.StartsWith(";") OrElse line.StartsWith("#")
    End Function

    Private Sub EnsureInitialized()
        If IsInitialized Then Return

        Dim baseDir As String = AppContext.BaseDirectory
        Dim iniPath = Path.Combine(baseDir, IniFileName)
        Dim pluginsPath = Path.Combine(baseDir, PluginsFolderName)

        ' Track what needs to be created
        Dim iniCreated As Boolean = False
        Dim pluginsDirCreated As Boolean = False

        ' Create INI file if it doesn't exist
        If Not File.Exists(iniPath) Then
            iniCreated = CreateDefaultIni(iniPath)
        End If

        ' Create plugins directory if it doesn't exist
        If Not Directory.Exists(pluginsPath) Then
            pluginsDirCreated = CreatePluginsDirectory(pluginsPath)
        End If

        ' If either was created, exit with appropriate message
        If iniCreated OrElse pluginsDirCreated Then
            ExitWithSetupMessage(iniCreated, pluginsDirCreated, iniPath, pluginsPath)
        End If

        ParseAllSections(iniPath)
        InitializeSectionLoaders()
        IsInitialized = True
    End Sub

    Private Function CreateDefaultIni(path As String) As Boolean
        Dim lines = GetDefaultIniContent()

        Try
            File.WriteAllLines(path, lines)

            ' Verify the file now exists (sanity check)
            If Not File.Exists(path) Then
                Console.WriteLine("ERROR: Failed to create INI file - file does not exist after write attempt.")
                Console.WriteLine($"       Attempted path: {path}")
                Environment.Exit(1)
            End If

            ' Success - file was created
            Return True

        Catch ex As UnauthorizedAccessException
            Console.WriteLine("ERROR: Permission denied while creating INI file.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(2)
        Catch ex As DirectoryNotFoundException
            Console.WriteLine("ERROR: Directory not found while creating INI file.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(3)
        Catch ex As IOException
            Console.WriteLine("ERROR: I/O error while creating INI file.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(4)
        Catch ex As Exception
            Console.WriteLine("ERROR: Unexpected error while creating INI file.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(5)
        End Try

        Return False ' Won't reach here due to Environment.Exit, but needed for compiler
    End Function

    Private Function CreatePluginsDirectory(path As String) As Boolean
        Try
            Directory.CreateDirectory(path)

            ' Verify the directory now exists (sanity check)
            If Not Directory.Exists(path) Then
                Console.WriteLine("ERROR: Failed to create plugins directory - directory does not exist after creation attempt.")
                Console.WriteLine($"       Attempted path: {path}")
                Environment.Exit(6)
            End If

            ' Success - directory was created
            Return True

        Catch ex As UnauthorizedAccessException
            Console.WriteLine("ERROR: Permission denied while creating plugins directory.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(7)
        Catch ex As PathTooLongException
            Console.WriteLine("ERROR: Path too long while creating plugins directory.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(8)
        Catch ex As IOException
            Console.WriteLine("ERROR:  I/O error while creating plugins directory.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(9)
        Catch ex As Exception
            Console.WriteLine("ERROR: Unexpected error while creating plugins directory.")
            Console.WriteLine($"       Path: {path}")
            Console.WriteLine($"       Details: {ex.Message}")
            Environment.Exit(10)
        End Try

        Return False ' Won't reach here due to Environment.Exit, but needed for compiler
    End Function

    Private Sub ExitWithSetupMessage(iniCreated As Boolean, pluginsDirCreated As Boolean, iniPath As String, pluginsPath As String)
        Console.WriteLine()
        Console.WriteLine("================================================================================")
        Console.WriteLine("                        FIRST-RUN SETUP COMPLETED")
        Console.WriteLine("================================================================================")
        Console.WriteLine()

        If iniCreated AndAlso pluginsDirCreated Then
            Console.WriteLine("The following required resources were missing and have been created:")
            Console.WriteLine()
            Console.WriteLine($"  1. Configuration file: {iniPath}")
            Console.WriteLine($"  2. Plugins directory:  {pluginsPath}")
            Console.WriteLine()
            Console.WriteLine("WHY IS THE APPLICATION EXITING?")
            Console.WriteLine("  Both the configuration file and plugins directory were just created.")
            Console.WriteLine("  The application cannot run without configured commands and plugins.")
            Console.WriteLine()
            Console.WriteLine("NEXT STEPS:")
            Console.WriteLine("  1. Edit the configuration file to customize command mappings and settings")
            Console.WriteLine("  2. Place your plugin assemblies (. dll files) in the plugins directory")
            Console.WriteLine("  3. Restart the application")
        ElseIf iniCreated Then
            Console.WriteLine("The configuration file was missing and has been created:")
            Console.WriteLine()
            Console.WriteLine($"  Configuration file: {iniPath}")
            Console.WriteLine()
            Console.WriteLine("WHY IS THE APPLICATION EXITING? ")
            Console.WriteLine("  A default configuration file was just created.")
            Console.WriteLine("  You should review and customize it before the application runs.")
            Console.WriteLine()
            Console.WriteLine("NEXT STEPS:")
            Console.WriteLine("  1. Edit the configuration file to customize command mappings and settings")
            Console.WriteLine("  2. Restart the application")
        ElseIf pluginsDirCreated Then
            Console.WriteLine("The plugins directory was missing and has been created:")
            Console.WriteLine()
            Console.WriteLine($"  Plugins directory: {pluginsPath}")
            Console.WriteLine()
            Console.WriteLine("WHY IS THE APPLICATION EXITING?")
            Console.WriteLine("  The plugins directory was just created and is currently empty.")
            Console.WriteLine("  The application requires plugins to provide functionality.")
            Console.WriteLine()
            Console.WriteLine("NEXT STEPS:")
            Console.WriteLine("  1. Place your plugin assemblies (.dll files) in the plugins directory")
            Console.WriteLine("  2. Restart the application")
        End If

        Console.WriteLine()
        Console.WriteLine("================================================================================")
        Console.WriteLine()

        Environment.Exit(0)
    End Sub

    Private Sub ParseAllSections(iniPath As String)
        Dim currentSection As String = Nothing
        Dim currentLines As List(Of String) = Nothing

        For Each raw In File.ReadAllLines(iniPath)
            Dim line = raw.Trim()

            If line.StartsWith("[") AndAlso line.EndsWith("]") Then
                currentSection = line.Substring(1, line.Length - 2).Trim()
                currentLines = New List(Of String)()
                SectionData(currentSection) = currentLines
                Continue For
            End If

            If currentLines IsNot Nothing Then
                currentLines.Add(raw)
            End If
        Next
    End Sub
End Module