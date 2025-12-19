Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.IO
Imports System.Net
Imports System.Numerics
Imports System.Text
Imports System.Text.Json
Imports System.Threading
Imports Current.PluginApi ' Namespace from your DLL




Public Structure IntersectionHistory
    Public Current As (Double, Double, Double)
    Public Previous As (Double, Double, Double)

    Public Sub New(current As (Double, Double, Double), previous As (Double, Double, Double))
        Me.Current = current
        Me.Previous = previous
    End Sub
End Structure





Public Module Module1


    Private requiredStationaryFrames As Integer = IniManager.GetSectionInt("Tuning", "RequiredStationaryFrames", 5)
    'Private requiredStationaryFrames As Integer = 5 ' <--- Set to whatever you like
    Private stationaryFrameCount As Integer = 0
    Private lastInputCoordinates As (Integer, Integer, Integer) = (-250, 155, -78)
    Private lastRawInputCoordinates As (Integer, Integer, Integer) = (-250, 155, -78)

    Public userCoordinates As (Integer, Integer, Integer) = (-250, 155, -78) '(-250, 155, -78)    -250    73  -78 
    Public observedLocation As String = "-370,73,-200"

    Public spatialZoneDict As New Dictionary(Of String, SpatialZone)


    Public panelData As New PanelDataManager
    Public marginMgr As New Margins.MarginManager()




    ' --- Hot bucket and bump-sort system ---

    ' All triangle set IDs ever seen
    Private allSetIds As New ConcurrentDictionary(Of Integer, Byte)()

    ' Set IDs that were “hot” this frame (blocked at least one ray this frame)
    Private hotSetIds As New ConcurrentDictionary(Of Integer, Byte)()

    ' Long-lived score per setId (aggregated & decayed over time)
    Private triangleSetSortScores As New ConcurrentDictionary(Of Integer, Integer)()

    ' Per-frame sorted list of setIds (highest score first)
    Private pregeneratedSortedSetIds As List(Of Integer) = Nothing

    ' Per-frame decay controls (no hard reset)
    Private SCORE_BUMP_ON_BLOCK As Integer = IniManager.GetSectionInt("Tuning", "ScoreBumpOnBlock", 8)      ' how much to add when a set blocks
    Private SCORE_DECAY_PER_FRAME As Integer = IniManager.GetSectionInt("Tuning", "ScoreDecayPerFrame", 1)  ' how much to subtract each frame

    ' Concurrent triangle data structures
    Public triangleGroups As New ConcurrentDictionary(Of Integer, ConcurrentBag(Of Integer))()
    Public trianglesById As New ConcurrentDictionary(Of Integer, Triangle)()

    ' Used only to track which setIds currently exist (if you need it)
    Public sortedSetIds As New ConcurrentDictionary(Of Integer, Byte)()

    ' --- Reusable buffers for per-frame sorting ---
    ' These avoid allocating new Dictionary and List every frame.
    Private sortScoresBuffer As New Dictionary(Of Integer, Integer)()
    Private sortedSetIdsBuffer As New List(Of Integer)()





    ' Removes all objects for a given structureId
    Sub RemoveObjectsByStructureId(structureId As Integer)
        Dim objectIds As ImmutableList(Of Integer) = Nothing
        If structureObjectIDs.TryGetValue(structureId, objectIds) Then
            For Each objId In objectIds
                objectDictionary.TryRemove(objId, Nothing)
            Next
            structureObjectIDs.TryRemove(structureId, objectIds)
        End If
        structureDrawState.TryRemove(structureId, Nothing)
    End Sub


    Sub PrintAllMargins(marginMgr As Margins.MarginManager)
        Dim marginIDs = marginMgr.GetAllMarginIDs()
        Console.WriteLine("Row Margins: " & String.Join(", ", marginIDs("row")))
        Console.WriteLine("Column Margins: " & String.Join(", ", marginIDs("column")))
    End Sub











    Public observerLocation As String = ""
    Public mccommand As String = "dummy data"
    Dim mccommandidemp As String = "clear"
    Dim rightclickcoord As String = ""
    Dim rightlickidemp As String = "clear"
    Dim leftclickcoord As String = ""
    Dim leftlickidemp As String = "clear"

    Private lastMccommandIdemp As String = ""
    Private lastRightclickIdemp As String = ""
    Private lastLeftclickIdemp As String = ""















    ' The specific set/margin IDs are local implementation details – not exposed.
    Public Sub CreateDefaultMarginSetsOnce()
        ' If either default set already exists, assume both exist and bail out.
        Dim existing = marginMgr.GetAllMarginSetNames()
        If existing IsNot Nothing AndAlso
       existing.Contains("DefaultTopSet") AndAlso
       existing.Contains("DefaultBottomSet") Then
            Exit Sub
        End If

        ' --- Top panel default margins at (0,0,0,0) ---
        marginMgr.CreateMargin("DefaultTop_TopRow", MarginType.RowMargin, PanelType.TopPanel, 0, Nothing, False)
        marginMgr.CreateMargin("DefaultTop_BottomRow", MarginType.RowMargin, PanelType.TopPanel, 0, Nothing, False)
        marginMgr.CreateMargin("DefaultTop_LeftColumn", MarginType.ColumnMargin, PanelType.TopPanel, Nothing, 0, False)
        marginMgr.CreateMargin("DefaultTop_RightColumn", MarginType.ColumnMargin, PanelType.TopPanel, Nothing, 0, False)

        marginMgr.CreateMarginSet(
        "DefaultTopSet",
        "DefaultTop_TopRow",
        "DefaultTop_BottomRow",
        "DefaultTop_LeftColumn",
        "DefaultTop_RightColumn"
    )

        ' --- Bottom panel default margins at (0,0,0,0) ---
        marginMgr.CreateMargin("DefaultBottom_TopRow", MarginType.RowMargin, PanelType.BottomPanel, 0, Nothing, False)
        marginMgr.CreateMargin("DefaultBottom_BottomRow", MarginType.RowMargin, PanelType.BottomPanel, 0, Nothing, False)
        marginMgr.CreateMargin("DefaultBottom_LeftColumn", MarginType.ColumnMargin, PanelType.BottomPanel, Nothing, 0, False)
        marginMgr.CreateMargin("DefaultBottom_RightColumn", MarginType.ColumnMargin, PanelType.BottomPanel, Nothing, 0, False)

        marginMgr.CreateMarginSet(
        "DefaultBottomSet",
        "DefaultBottom_TopRow",
        "DefaultBottom_BottomRow",
        "DefaultBottom_LeftColumn",
        "DefaultBottom_RightColumn"
    )
    End Sub














    Public thinnedDict As New ConcurrentDictionary(Of Integer, MyObject)



    ' Response handling for HTTP server
    Private responseString As String = ""
    Private responseReady As Boolean = False
    Private processingLock As New Object()
    Private processingCompletion As New ManualResetEvent(False)
    Dim processingTask As Task = Nothing

    Sub Main()


        CreateDefaultMarginSetsOnce()


        'scattering()


        'CreateGridLinesGrid(-250, 70, -78, 90210, 10, 10, 2000, 1000)
        'Console.WriteLine("created grid lines")
        'Console.WriteLine(objectDictionary.Count)
        '      ThinEvenSpatiallyAdaptiveAuto(objectDictionary, thinnedDict, 50000, userCoordinates, 500, 10, 20)
        'Console.WriteLine("Thinned objectDictionary")
        'Console.WriteLine(objectDictionary.Count)


        Console.WriteLine("pre-plugin 3D objects loaded")
        ' Create API implementation (for plugins to use)
        'CommandConfig.InitializeCommands()
        Dim apiImpl As CurrentApiImpl = New CurrentApiImpl(marginMgr)
        Dim pluginManager As PluginManager = New PluginManager()
        pluginManager.LoadPlugins()





        'PruneDistantObjects()




        InitializeFrameCaches()

        InitializeSortedSetIds()

        SyncLock processingLock
            responseReady = False
            processingCompletion.Reset()
        End SyncLock
        processingTask = Task.Run(Sub() ProcessData())

        ' Start HTTP server on its own thread
        Dim httpThread As New Thread(Sub() RunHttpServer(apiImpl, pluginManager, marginMgr))
        httpThread.IsBackground = True ' Optional: make it a background thread
        httpThread.Start()

        ' The main thread is now free to do other things (e.g., console UI, periodic updates, plugin launching, etc.)
        ' Example: You could add a simple loop to keep Main alive, or process other commands:
        While True
            ' You could add a console menu, periodic logging, etc.
            Thread.Sleep(1000)
        End While
    End Sub
    Sub RunHttpServer(apiImpl As CurrentApiImpl, pluginManager As PluginManager, marginMgr As Margins.MarginManager)
        Dim listener As New HttpListener()
        listener.Prefixes.Add("http://127.0.0.1:8080/")
        listener.Start()
        Console.WriteLine("Server started. Listening for requests...")

        While True
            Dim context As HttpListenerContext = listener.GetContext()
            Dim request As HttpListenerRequest = context.Request
            Dim response As HttpListenerResponse = context.Response

            If request.HttpMethod = "POST" AndAlso request.Url.LocalPath = "/data" Then
                Dim canRespondNow As Boolean

                SyncLock processingLock
                    canRespondNow = responseReady
                End SyncLock

                If Not canRespondNow Then
                    response.StatusCode = 429 ' 429-Client is making requests too fast.
                    Dim errBytes = Encoding.UTF8.GetBytes("Server busy or not ready")
                    response.ContentLength64 = errBytes.Length
                    response.OutputStream.Write(errBytes, 0, errBytes.Length)
                    response.OutputStream.Close()
                    response.Close()
                    Continue While
                End If



                Using reader As New StreamReader(request.InputStream, Encoding.UTF8)
                    Dim jsonString As String = reader.ReadToEnd()
                    Dim jsonDocument As JsonDocument = JsonDocument.Parse(jsonString)
                    Dim rootElement As JsonElement = jsonDocument.RootElement

                    observerLocation = JsonElementToString(rootElement.GetProperty("observerLocation"))
                    observedLocation = JsonElementToString(rootElement.GetProperty("observedLocation"))
                    mccommand = JsonElementToString(rootElement.GetProperty("mccommand"))
                    mccommandidemp = JsonElementToString(rootElement.GetProperty("mccommandidemp"))
                    rightclickcoord = JsonElementToString(rootElement.GetProperty("rightclickcoord"))
                    rightlickidemp = JsonElementToString(rootElement.GetProperty("rightlickidemp"))
                    leftclickcoord = JsonElementToString(rootElement.GetProperty("leftclickcoord"))
                    leftlickidemp = JsonElementToString(rootElement.GetProperty("leftlickidemp"))
                End Using


                'If Not mccommandidemp = "clear" AndAlso Not mccommandidemp = "dummy data" AndAlso Not mccommandidemp = lastMccommandIdemp Then
                '    Console.WriteLine("command idemp")
                '    Console.WriteLine(mccommandidemp)
                '    lastMccommandIdemp = mccommandidemp
                'End If

                ' Right Click Idemp
                If Not rightlickidemp = "clear" AndAlso Not rightlickidemp = "dummy data" AndAlso Not rightlickidemp = lastRightclickIdemp Then
                    Console.WriteLine("right click idemp")
                    Console.WriteLine(rightlickidemp)
                    lastRightclickIdemp = rightlickidemp
                End If

                ' Left Click Idemp
                If Not leftlickidemp = "clear" AndAlso Not leftlickidemp = "dummy data" AndAlso Not leftlickidemp = lastLeftclickIdemp Then
                    Console.WriteLine("left click idemp")
                    Console.WriteLine(leftlickidemp)
                    lastLeftclickIdemp = leftlickidemp
                End If

                Dim buffer As Byte() = Encoding.UTF8.GetBytes(responseString)
                response.ContentLength64 = buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)
                response.OutputStream.Close()
                response.Close()

                ' Ready: handle request 
                'HandleMCCommand(zone, apiImpl, pluginManager, marginMgr)
                'HandleMCCommand(apiImpl, pluginManager, marginMgr)


                If Not String.IsNullOrEmpty(mccommandidemp) AndAlso mccommandidemp <> "clear" AndAlso mccommandidemp <> lastMccommandIdemp Then
                    'Console.WriteLine(mccommand)
                    HandleMCCommand(apiImpl, pluginManager, marginMgr)
                    lastMccommandIdemp = mccommandidemp
                    ' Optionally: clear mccommand value here if you want
                    mccommand = "dummy data"
                End If




                fillUserCoordinates(observerLocation)

                'Console.WriteLine("[DEBUG] observerLocation: " & observerLocation)
                'Console.WriteLine("[DEBUG] observedLocation: " & observedLocation)
                'Console.WriteLine("[DEBUG] userCoordinates: " & userCoordinates.ToString())
                ' |== Cursor ==|
                ' Save and compute all info under the "last" key (or use session/user id if desired)
                'UpdateObserverVectorData(userCoordinates, observedLocation)
                'PrintObserverVectorData()
                ' |============|

                ' Trigger pre-processing of the next frame
                SyncLock processingLock
                    responseReady = False
                    processingCompletion.Reset()
                End SyncLock
                processingTask = Task.Run(Sub() ProcessData())

            Else
                response.StatusCode = 404
                response.Close()
            End If
        End While
    End Sub

    Private Function JsonElementToString(element As JsonElement) As String
        Select Case element.ValueKind
            Case JsonValueKind.String
                Return element.GetString()
            Case JsonValueKind.Number
                Return element.GetRawText() ' ToString() on JsonElement returns the type name, use GetRawText() for the numeric value
            Case JsonValueKind.Null
                Return ""
            Case Else
                Return element.ToString()
        End Select
    End Function














    Sub fillUserCoordinates(playerData As String)
        Dim s As String = New String(playerData)
        Dim haveFloat As String() = s.Split(New Char() {","c})
        lastRawInputCoordinates.Item1 = CInt(haveFloat(0))
        lastRawInputCoordinates.Item2 = CInt(haveFloat(1)) + 2 ' Adding 2 to Y for "player height"
        lastRawInputCoordinates.Item3 = CInt(haveFloat(2))
    End Sub



    Public structureDrawState As New ConcurrentDictionary(Of Integer, Boolean)
    Private objectsIntersectionDict As New ConcurrentDictionary(Of Integer, IntersectionHistory)

    Private previousFrameColorDict As New Dictionary(Of (Double, Double, Double), ObjectColor)()
    Public Sub ProcessData()

        ' Detect observer settling
        If lastRawInputCoordinates.Equals(lastInputCoordinates) Then
            stationaryFrameCount += 1
            If stationaryFrameCount >= requiredStationaryFrames Then
                userCoordinates = lastRawInputCoordinates
            End If
        Else
            stationaryFrameCount = 1 ' Reset to 1 since this is the first matching frame after movement
        End If
        lastInputCoordinates = lastRawInputCoordinates

        UpdateObserverVectorData(userCoordinates, observedLocation)

        ' Take fast, efficient snapshots as regular dictionaries (not arrays, not immutable)
        Dim objectDictSnapshot = New Dictionary(Of Integer, MyObject)(objectDictionary)
        Dim trianglesByIdSnapshot = New Dictionary(Of Integer, Triangle)(trianglesById)
        Dim structureDrawStateSnapshot = New Dictionary(Of Integer, Boolean)(structureDrawState)
        Dim triangleGroupsSnapshot = New Dictionary(Of Integer, ConcurrentBag(Of Integer))(triangleGroups)
        Dim structureObjectIDsSnapshot = New Dictionary(Of Integer, Immutable.ImmutableList(Of Integer))(structureObjectIDs)

        responseString = GenerateResponseString(
        objectDictSnapshot,
        trianglesByIdSnapshot,
        structureDrawStateSnapshot,
        triangleGroupsSnapshot,
        structureObjectIDsSnapshot
    )


        ' Render debug info about triangle sorting to console (HUD)
        ' ########################################################################## RenderTriangleSortDebugOverlay()
        'RenderTriangleSortDebugOverlay()

        SyncLock processingLock
            responseReady = True
            processingCompletion.Set()
        End SyncLock

        ' Show the response string as it is in its deployed form
        'Console.WriteLine(responseString)
    End Sub











    ' --- Module-level caches (declare these at the top of your Module, outside any function) ---
    ' Cached static array of PanelType enum values
    Private CachedPanelTypeValues As PanelType() = CType([Enum].GetValues(GetType(PanelType)), PanelType())

    ' Cached static PanelBounds (if panel geometry is static)
    Private CachedPanelBounds As PanelBounds = Nothing

    ' Cached StringBuilder dictionary for each ObjectColor
    Private CachedColorBuilders As Dictionary(Of ObjectColor, StringBuilder) = Nothing


    Public Sub InitializeFrameCaches()
        ' Cache PanelBounds (after panelData is ready)
        CachedPanelBounds = New PanelBounds()

        ' Get all enum values as an array
        Dim colors As ObjectColor() = DirectCast([Enum].GetValues(GetType(ObjectColor)), ObjectColor())

        ' Create the dictionary with a known capacity
        Dim dict As New Dictionary(Of ObjectColor, StringBuilder)(colors.Length)

        ' Populate the dictionary
        For Each color As ObjectColor In colors
            dict(color) = New StringBuilder()
        Next

        ' Assign to the cache
        CachedColorBuilders = dict
    End Sub








    Public Function GenerateResponseString(
    objectDictSnap As Dictionary(Of Integer, MyObject),
    trianglesByIdSnap As Dictionary(Of Integer, Triangle),
    structureDrawState As Dictionary(Of Integer, Boolean),
    triangleGroups As Dictionary(Of Integer, ConcurrentBag(Of Integer)),
    structureObjectIDs As Dictionary(Of Integer, Immutable.ImmutableList(Of Integer))
) As String

        '-----------------------------------------------------------------------
        ' INITIALIZATION — cache pulls and per-call containers
        '-----------------------------------------------------------------------
        ' Purpose: Prime cached values and allocate per-call, thread-safe containers.
        ' Consumes: None (reads global caches).
        ' Produces: PanelTypeValues/Count, bounds, colorBuilders cleared, and per-frame dicts.

        ' Use cached PanelTypeValues and PanelBounds
        Dim PanelTypeValues As PanelType() = CachedPanelTypeValues
        Dim PanelTypeCount As Integer = PanelTypeValues.Length
        Dim bounds As PanelBounds = CachedPanelBounds

        ' Clear and reuse the cached colorBuilders StringBuilders
        For Each sb In CachedColorBuilders.Values
            sb.Clear()
        Next
        Dim colorBuilders As Dictionary(Of ObjectColor, StringBuilder) = CachedColorBuilders

        ' Use local dictionaries for parallel processing
        Dim tempObjectsIntersectionDict As New ConcurrentDictionary(Of Integer, IntersectionHistory)()
        'Dim blockedStructureIds As New ConcurrentDictionary(Of Integer, Byte)()
        Dim blockingTriangleSetIds As New ConcurrentDictionary(Of Integer, Byte)()
        Dim coordToColorOverrideObjId As New ConcurrentDictionary(Of (Double, Double, Double), Integer)()

        '-----------------------------------------------------------------------
        ' FRAME PREPARATION — hot-bucket and set-id prep
        '-----------------------------------------------------------------------
        ' Purpose: Use scores and hot sets from the PREVIOUS frame to decide
        ' this frame's set ordering, then clear hot flags so this frame can
        ' accumulate a fresh set of hot sets.
        ' Requires: triangleGroups available.
        ' Produces: pregeneratedSortedSetIds ready for this frame.


        ' Reset per-frame debug counters
        raysThisFrame = 0
        blocksThisFrame = 0
        perSetBlocksThisFrame.Clear()

        ' Build per-frame sorted setIds based on decayed scores + last frame's hits.
        ' Uses global triangleGroups / triangleSetSortScores / hotSetIds.
        PrepareSortedSetIdsForFrame()

        '-----------------------------------------------------------------------
        ' PARALLEL OBJECT PROCESSING — visibility and intersection per object
        '-----------------------------------------------------------------------
        ' Purpose: For each object, determine the nearest valid panel intersection
        ' visible to the observer.
        ' Requires: structureDrawState, trianglesByIdSnap, panelData, bounds, userCoordinates.
        ' Produces: tempObjectsIntersectionDict, coordToColorOverrideObjId, blockingTriangleSetIds.

        Parallel.ForEach(objectDictSnap, Sub(objectKvp)
                                             '-------------------------------------------------------------------
                                             ' OBJECT PROPERTY EXTRACTION — pull object/user positions
                                             '-------------------------------------------------------------------
                                             ' Prerequisites: objectKvp
                                             ' Responsibility: Extract object coordinates and observer coordinates.
                                             ' Results: x,y,z (object coords), ux,uy,uz (observer coords), positions.

                                             Dim objectID = objectKvp.Key
                                             Dim obj = objectKvp.Value

                                             Dim x As Long = obj.Location.X
                                             Dim y As Long = obj.Location.Y
                                             Dim z As Long = obj.Location.Z

                                             Dim ux As Integer = userCoordinates.Item1
                                             Dim uy As Integer = userCoordinates.Item2
                                             Dim uz As Integer = userCoordinates.Item3

                                             Dim objectPos = (CDbl(x), CDbl(y), CDbl(z))
                                             Dim observerPos = (CDbl(ux), CDbl(uy), CDbl(uz))

                                             '-------------------------------------------------------------------
                                             ' STRUCTURE VISIBILITY CHECK — skip if structure is off
                                             '-------------------------------------------------------------------
                                             ' Prerequisites: object properties
                                             ' Responsibility: Check if object's structure is toggled visible.
                                             ' Results: Early exit if structure is not visible.

                                             Dim structureOnNow As Boolean = True
                                             structureDrawState.TryGetValue(obj.StructureId, structureOnNow)

                                             ' EARLY EXIT: If structure is not on, skip all ray and intersection logic
                                             If Not structureOnNow Then
                                                 'blockedStructureIds.TryAdd(obj.StructureId, 0)
                                                 Return
                                             End If

                                             '-------------------------------------------------------------------
                                             ' RAY OBSTRUCTION CHECK — line-of-sight via triangles
                                             '-------------------------------------------------------------------
                                             ' Prerequisites: object and observer positions
                                             ' Responsibility: Determine if any triangle occludes the ray.
                                             ' Results: Early exit if blocked (records blocking set ID when present).

                                             ' Use the current global triangle data (triangles move each frame).
                                             ' The frame's ordering (pregeneratedSortedSetIds) was prepared above.
                                             Dim rayResult = IsRayObscuredByAnyTriangle(observerPos, objectPos)

                                             ' EARLY EXIT: If ray is blocked, skip all intersection logic
                                             If rayResult.Blocked Then
                                                 'blockedStructureIds.TryAdd(obj.StructureId, 0)
                                                 If rayResult.BlockedSetId >= 0 Then
                                                     blockingTriangleSetIds.TryAdd(rayResult.BlockedSetId, 0)
                                                 End If
                                                 Return
                                             End If

                                             '-------------------------------------------------------------------
                                             ' PANEL INTERSECTION PROCESSING — find best hit
                                             '-------------------------------------------------------------------
                                             ' Prerequisites: object is visible and not obscured
                                             ' Responsibility: Compute candidate panel intersections and pick best.
                                             ' Results: bestCoord (best intersection point) or early exit.

                                             Dim objCoord As (Integer, Integer, Integer) = (CInt(x), CInt(y), CInt(z))
                                             Dim observerCoord As (Integer, Integer, Integer) = (ux, uy, uz)

                                             ' Compute panel intersections (no thread-local panel arrays needed)
                                             Dim result = PanelHelpers.GetHolodecPanelIntersections(observerCoord, objCoord, panelData, PanelTypeCount)
                                             Dim intersections = result.Item1
                                             Dim intersectionsValidMask As Integer = result.Item2
                                             If intersectionsValidMask = 0 Then Return

                                             ' Single-pass evaluation to avoid extra loops and allocations
                                             Dim distObjToUser As Long =
                                             CLng(ux - x) * CLng(ux - x) +
                                             CLng(uy - y) * CLng(uy - y) +
                                             CLng(uz - z) * CLng(uz - z)

                                             Dim minDistance As Single = Single.MaxValue
                                             Dim bestCoord = (0, 0, 0)
                                             Dim foundAny As Boolean = False

                                             For i = 0 To PanelTypeCount - 1
                                                 If (intersectionsValidMask And (1 << i)) = 0 Then Continue For

                                                 Dim panelType = PanelTypeValues(i)
                                                 Dim v3 = intersections(i)
                                                 Dim t = v3.ToTuple()

                                                 ' Pre-round once per candidate
                                                 Dim rx As Integer = CInt(Math.Round(t.x))
                                                 Dim ry As Integer = CInt(Math.Round(t.y))
                                                 Dim rz As Integer = CInt(Math.Round(t.z))

                                                 Dim snappedPoint As (Integer, Integer, Integer)
                                                 Select Case panelType
                                                     Case PanelType.TopPanel
                                                         snappedPoint = (rx, panelData.Top_a.Item2, rz)
                                                     Case PanelType.BottomPanel
                                                         snappedPoint = (rx, panelData.Bottom_a.Item2, rz)
                                                     Case PanelType.NorthPanel
                                                         snappedPoint = (rx, ry, panelData.North_a.Item3)
                                                     Case PanelType.SouthPanel
                                                         snappedPoint = (rx, ry, panelData.South_a.Item3)
                                                     Case PanelType.EastPanel
                                                         snappedPoint = (panelData.East_a.Item1, ry, rz)
                                                     Case PanelType.WestPanel
                                                         snappedPoint = (panelData.West_a.Item1, ry, rz)
                                                     Case Else
                                                         snappedPoint = (rx, ry, rz)
                                                 End Select

                                                 ' Bounds check per panel type
                                                 If Not bounds.IsPointWithinPanel(panelType, snappedPoint) Then Continue For

                                                 ' Distance gate (object -> intersection must be <= object -> user)
                                                 Dim dx As Long = CLng(snappedPoint.Item1) - x
                                                 Dim dy As Long = CLng(snappedPoint.Item2) - y
                                                 Dim dz As Long = CLng(snappedPoint.Item3) - z
                                                 Dim distObjToIntersect As Long = dx * dx + dy * dy + dz * dz
                                                 If distObjToIntersect > distObjToUser Then Continue For

                                                 ' Candidate passes gating; compute fast distance for tie-breaking
                                                 Dim dist As Single = CalculateFastDistance((x, y, z), snappedPoint)
                                                 If dist < minDistance Then
                                                     minDistance = dist
                                                     bestCoord = snappedPoint
                                                     foundAny = True
                                                 End If
                                             Next

                                             ' EARLY EXIT: If no points pass all tests
                                             If Not foundAny Then Return

                                             '-------------------------------------------------------------------
                                             ' INTERSECTION RECORDING — store hit and color override
                                             '-------------------------------------------------------------------
                                             ' Prerequisites: best intersection point found
                                             ' Responsibility: Record intersection and any color override owner.
                                             ' Results: tempObjectsIntersectionDict and coordToColorOverrideObjId updated.

                                             Dim objId = obj.UniqIdentifier
                                             Dim oldRecord As IntersectionHistory

                                             ' Use frozen snapshot of previous intersections from global dict
                                             If Not objectsIntersectionDict.TryGetValue(objId, oldRecord) Then
                                                 ' For first-time objects, "previous" == current
                                                 oldRecord = New IntersectionHistory(
                                                 (CDbl(bestCoord.Item1), CDbl(bestCoord.Item2), CDbl(bestCoord.Item3)),
                                                 (CDbl(bestCoord.Item1), CDbl(bestCoord.Item2), CDbl(bestCoord.Item3))
                                             )
                                             End If

                                             Dim newCurrent As (Double, Double, Double) =
                                             (CDbl(bestCoord.Item1), CDbl(bestCoord.Item2), CDbl(bestCoord.Item3))

                                             Dim newRec As New IntersectionHistory(
                                             newCurrent,
                                             oldRecord.Current
                                         )

                                             tempObjectsIntersectionDict(objId) = newRec

                                             If obj.ColorOverride IsNot Nothing Then
                                                 coordToColorOverrideObjId.TryAdd(newCurrent, obj.UniqIdentifier)
                                             End If
                                         End Sub)

        '-----------------------------------------------------------------------
        ' POST-PROCESSING — merge thread-local intersections
        '-----------------------------------------------------------------------
        ' Prerequisites: tempObjectsIntersectionDict populated
        ' Responsibility: Merge parallel results and update shared state
        ' Results: objectsIntersectionDict updated with merged results

        For Each tempIntersectionEntry In tempObjectsIntersectionDict
            objectsIntersectionDict(tempIntersectionEntry.Key) = tempIntersectionEntry.Value
        Next

        '-----------------------------------------------------------------------
        ' COLOR DIFFERENCE ENGINE — minimal per-frame delta output
        '-----------------------------------------------------------------------
        ' Prerequisites: objectsIntersectionDict populated
        ' Responsibility: Calculate color deltas vs. previous frame and build output
        ' Results: String representing only changed/erased coordinates for rendering

        Dim currentFrameColorDict As New Dictionary(Of (Double, Double, Double), ObjectColor)()

        ' PATCH: Only loop over visible blocks for color assignment!
        For Each colorEntry In tempObjectsIntersectionDict
            Dim objId = colorEntry.Key
            Dim curr = colorEntry.Value.Current
            Dim forced As ObjectColor? = Nothing
            Dim forcedObjId As Integer = -1
            If coordToColorOverrideObjId.TryGetValue(curr, forcedObjId) Then
                Dim forcedObj As MyObject = Nothing
                ' Use the frame snapshot for color lookup to keep frame integrity
                If objectDictSnap.TryGetValue(forcedObjId, forcedObj) Then
                    forced = forcedObj.ColorOverride
                End If
            End If
            Dim colorToUse As ObjectColor = If(forced.HasValue, forced.Value, ObjectColor.White)
            currentFrameColorDict(curr) = colorToUse ' Use assignment rather than TryAdd
        Next

        ' Only output new, erased, or color-changed blocks
        For Each currFrameEntry In currentFrameColorDict
            Dim coord = currFrameEntry.Key
            Dim colorNow = currFrameEntry.Value
            Dim colorPrev As ObjectColor = ObjectColor.Black
            Dim hadPrev = previousFrameColorDict.TryGetValue(coord, colorPrev)
            If (Not hadPrev) OrElse (colorPrev <> colorNow) Then
                colorBuilders(colorNow).Append($"{coord.Item1},{coord.Item2},{coord.Item3};")
            End If
        Next

        For Each coord In previousFrameColorDict.Keys
            If Not currentFrameColorDict.ContainsKey(coord) Then
                colorBuilders(ObjectColor.Black).Append($"{coord.Item1},{coord.Item2},{coord.Item3};")
            End If
        Next

        previousFrameColorDict = New Dictionary(Of (Double, Double, Double), ObjectColor)(currentFrameColorDict)

        Return TestBlocksPlusColors(colorBuilders)
    End Function



    Public allFrameTuples As New List(Of Tuple(Of StringBuilder, StringBuilder))


    Private Function TestBlocksPlusColors(colorBuilders As Dictionary(Of ObjectColor, StringBuilder)) As String
        Dim sb As New StringBuilder()

        ' we put our idemp flags here

        sb.Append(mccommandidemp)
        sb.Append("!")
        sb.Append(rightlickidemp)
        sb.Append("!")
        sb.Append(leftlickidemp)

        sb.Append("BEGIN")

        ' — BLACK segment and its original test blocks —
        sb.Append("%")
        sb.Append(colorBuilders(ObjectColor.Black).ToString())
        sb.Append("-370,73,-200;-370,72,-200")

        ' — WHITE segment and its original test blocks —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.White).ToString())
        sb.Append("-370,75,-200;-370,74,-200")

        ' — YELLOW segment and its original test blocks —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Yellow).ToString())
        sb.Append("-370,71,-200;-370,70,-200")

        ' — WHITEDIM segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.WhiteDim).ToString())
        sb.Append("-370,75,-200;-370,74,-200") ' using white coords for now

        ' — GREYLIGHT segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.GreyLight).ToString())
        sb.Append("-370,71,-200;-370,70,-200") ' using yellow coords for now

        ' — GREY segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Grey).ToString())
        sb.Append("-370,73,-200;-370,72,-200") ' using black coords for now

        ' — BROWN segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Brown).ToString())
        sb.Append("-370,75,-200;-370,74,-200") ' using white coords for now

        ' — RED segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Red).ToString())
        sb.Append("-370,71,-200;-370,70,-200") ' using yellow coords for now

        ' — ORANGE segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Orange).ToString())
        sb.Append("-370,73,-200;-370,72,-200") ' using black coords for now

        ' — LIME segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Lime).ToString())
        sb.Append("-370,75,-200;-370,74,-200") ' using white coords for now

        ' — GREEN segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Green).ToString())
        sb.Append("-370,71,-200;-370,70,-200") ' using yellow coords for now

        ' — CYAN segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Cyan).ToString())
        sb.Append("-370,73,-200;-370,72,-200") ' using black coords for now

        ' — BLUELIGHT segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.BlueLight).ToString())
        sb.Append("-370,75,-200;-370,74,-200") ' using white coords for now

        ' — BLUE segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Blue).ToString())
        sb.Append("-370,71,-200;-370,70,-200") ' using yellow coords for now

        ' — PURPLE segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Purple).ToString())
        sb.Append("-370,73,-200;-370,72,-200") ' using black coords for now

        ' — MAGENTA segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Magenta).ToString())
        sb.Append("-370,75,-200;-370,74,-200") ' using white coords for now

        ' — PINK segment —
        sb.Append("#")
        sb.Append(colorBuilders(ObjectColor.Pink).ToString())
        sb.Append("-370,71,-200;-370,70,-200") ' using yellow coords for now

        sb.Append("END")
        Return sb.ToString().Replace(vbCr, "").Replace(vbLf, "")
    End Function



    Public objectDictionary As New ConcurrentDictionary(Of Integer, MyObject)
    Public structureObjectIDs As New ConcurrentDictionary(Of Integer, Immutable.ImmutableList(Of Integer))()
    Public Function AddMyObjectToFactory(x As Integer, y As Integer, z As Integer, structureId As Integer) As Integer
        Dim key As Integer = GetNextUniqId()
        Dim location As New Coordinates3D(x, y, z)
        Dim obj As New MyObject(location, key, structureId)
        objectDictionary.TryAdd(key, obj)
        structureDrawState.TryAdd(structureId, True)
        Dim updated As Boolean = False
        Do While Not updated
            Dim currentList As ImmutableList(Of Integer) = Nothing
            If structureObjectIDs.TryGetValue(structureId, currentList) Then
                Dim newList As ImmutableList(Of Integer) = currentList.Add(key)
                updated = structureObjectIDs.TryUpdate(structureId, newList, currentList)
            Else
                Dim newList As ImmutableList(Of Integer) = ImmutableList.Create(key)
                updated = structureObjectIDs.TryAdd(structureId, newList)
            End If
        Loop

        Return key
    End Function








    ' Helper: Euclidean distance in 3D
    Private Function Distance3D(a As (Long, Long, Long), b As (Long, Long, Long)) As Double
        Dim dx = CDbl(a.Item1 - b.Item1)
        Dim dy = CDbl(a.Item2 - b.Item2)
        Dim dz = CDbl(a.Item3 - b.Item3)
        Return Math.Sqrt(dx * dx + dy * dy + dz * dz)
    End Function


    Public Sub ThinEvenSpatiallyAdaptiveAuto(
    ByRef sourceDict As ConcurrentDictionary(Of Integer, MyObject), ' Original object collection to be thinned
    ByRef destDict As ConcurrentDictionary(Of Integer, MyObject),   ' Storage for removed objects
    numToLeave As Integer,                                          ' Target number of objects to retain
    observer As (Integer, Integer, Integer),                        ' Reference point for distance calculations
    keepRadius As Double,                                           ' Distance threshold for automatic preservation
    Optional numBands As Integer = 10,                              ' Number of distance-based grouping bands
    Optional closeBiasExponent As Double = 1.5                      ' Weighting factor for proximity-based selection
)



        'DumpThinEvenSpatiallyAdaptiveAutoSignatureRequirements(
        '    sourceDict:=sourceDict,
        '    destDict:=destDict,
        '    numToLeave:=numToLeave,
        '    observer:=observer,
        '    keepRadius:=keepRadius,
        '    numBands:=numBands,
        '    closeBiasExponent:=closeBiasExponent
        ')



        If sourceDict.Count <= numToLeave OrElse numToLeave <= 0 Then Exit Sub

        Dim keys() As Integer = sourceDict.Keys.ToArray()
        If keys.Length = 0 Then Exit Sub

        ' 1. Calculate distances and assign bands
        Dim dists(keys.Length - 1) As Double
        Dim minDist As Double = Double.MaxValue
        Dim maxDist As Double = Double.MinValue
        For i = 0 To keys.Length - 1
            Dim obj As MyObject = sourceDict(keys(i))
            Dim d = Distance3D((CLng(obj.Location.X), CLng(obj.Location.Y), CLng(obj.Location.Z)), (CLng(observer.Item1), CLng(observer.Item2), CLng(observer.Item3)))
            dists(i) = d
            If d > keepRadius Then
                If d < minDist Then minDist = d
                If d > maxDist Then maxDist = d
            End If
        Next
        If minDist = Double.MaxValue Then minDist = keepRadius
        If maxDist = Double.MinValue Then maxDist = keepRadius + 1

        ' 2. Allocate objects to bands and record stats per band
        Dim bands(numBands - 1) As List(Of Integer)
        Dim bandCounts(numBands - 1) As Long
        Dim bandMinX(numBands - 1), bandMaxX(numBands - 1), bandMinY(numBands - 1), bandMaxY(numBands - 1), bandMinZ(numBands - 1), bandMaxZ(numBands - 1) As Long
        Dim bandFirst(numBands - 1) As Boolean
        For i = 0 To numBands - 1 : bands(i) = New List(Of Integer) : bandFirst(i) = True : Next

        For i = 0 To keys.Length - 1
            Dim d = dists(i)
            Dim bandIdx As Integer
            If d <= keepRadius Then
                bandIdx = 0
            Else
                bandIdx = 1 + CInt(Math.Floor((d - keepRadius) / Math.Max(1, (maxDist - keepRadius) / (numBands - 1))))
                If bandIdx >= numBands Then bandIdx = numBands - 1
            End If
            bands(bandIdx).Add(keys(i))
            bandCounts(bandIdx) += 1

            Dim obj As MyObject = sourceDict(keys(i))
            Dim x As Long = CLng(obj.Location.X)
            Dim y As Long = CLng(obj.Location.Y)
            Dim z As Long = CLng(obj.Location.Z)
            If bandFirst(bandIdx) Then
                bandMinX(bandIdx) = x : bandMaxX(bandIdx) = x
                bandMinY(bandIdx) = y : bandMaxY(bandIdx) = y
                bandMinZ(bandIdx) = z : bandMaxZ(bandIdx) = z
                bandFirst(bandIdx) = False
            Else
                If x < bandMinX(bandIdx) Then bandMinX(bandIdx) = x
                If x > bandMaxX(bandIdx) Then bandMaxX(bandIdx) = x
                If y < bandMinY(bandIdx) Then bandMinY(bandIdx) = y
                If y > bandMaxY(bandIdx) Then bandMaxY(bandIdx) = y
                If z < bandMinZ(bandIdx) Then bandMinZ(bandIdx) = z
                If z > bandMaxZ(bandIdx) Then bandMaxZ(bandIdx) = z
            End If
        Next

        ' 3. Compute "close-biased" weights per band (exponent >1 = more up close)
        Dim weights(numBands - 1) As Double
        Dim totalWeight As Double = 0
        For i = 0 To numBands - 1
            weights(i) = Math.Pow((numBands - i) / numBands, closeBiasExponent)
            totalWeight += weights(i) * bandCounts(i)
        Next

        ' 4. Allocate keep count per band
        Dim keepPerBand(numBands - 1) As Integer
        Dim assignedSoFar As Long = 0
        For i = 0 To numBands - 1
            If bandCounts(i) = 0 Then
                keepPerBand(i) = 0
            Else
                Dim rawKeep As Double = (weights(i) * bandCounts(i) / totalWeight) * numToLeave
                If rawKeep > Integer.MaxValue Then
                    keepPerBand(i) = Integer.MaxValue
                ElseIf rawKeep < 0 Then
                    keepPerBand(i) = 0
                Else
                    keepPerBand(i) = CInt(Math.Round(rawKeep))
                End If
            End If
            assignedSoFar += keepPerBand(i)
        Next
        ' Fix for rounding errors
        While assignedSoFar < numToLeave
            Dim maxIdx As Integer = Array.IndexOf(keepPerBand, keepPerBand.Max())
            If maxIdx = -1 Then Exit While
            keepPerBand(maxIdx) += 1
            assignedSoFar += 1
        End While
        While assignedSoFar > numToLeave
            Dim lastNonzero As Integer = Array.FindLastIndex(keepPerBand, Function(x) x > 0)
            If lastNonzero = -1 Then Exit While
            keepPerBand(lastNonzero) -= 1
            assignedSoFar -= 1
        End While

        ' 5. For each band, set grid size so nCells ≈ keepPerBand(i), then use that for binning
        Dim keepSet As New HashSet(Of Integer)
        Dim rnd As New Random()
        For bandIdx = 0 To numBands - 1
            Dim bandKeys = bands(bandIdx)
            If bandCounts(bandIdx) = 0 Then Continue For

            If bandIdx = 0 Then
                ' Always keep all in keepRadius
                For Each k In bandKeys
                    keepSet.Add(k)
                Next
            Else
                ' Compute bounding box for this band
                Dim dx As Long = Math.Max(1, bandMaxX(bandIdx) - bandMinX(bandIdx) + 1)
                Dim dy As Long = Math.Max(1, bandMaxY(bandIdx) - bandMinY(bandIdx) + 1)
                Dim dz As Long = Math.Max(1, bandMaxZ(bandIdx) - bandMinZ(bandIdx) + 1)
                Dim volume As Double = 1.0
                Try
                    volume = CDbl(dx) * CDbl(dy) * CDbl(dz)
                Catch ex As OverflowException
                    volume = Double.MaxValue
                End Try
                Dim targetCells As Double = Math.Max(1, keepPerBand(bandIdx))
                Dim gridSize As Double = Math.Max(1.0, Math.Pow(volume / targetCells, 1.0 / 3.0))

                ' Bin objects in this band by this grid
                Dim gridDict As New Dictionary(Of Tuple(Of Integer, Integer, Integer), List(Of Integer))()
                For Each idx In bandKeys
                    Dim obj As MyObject = sourceDict(idx)
                    ' Use Double for arithmetic, clamp to Int32 range
                    Dim xBin As Integer, yBin As Integer, zBin As Integer
                    Try
                        xBin = CInt(Math.Max(Integer.MinValue, Math.Min(Integer.MaxValue, Math.Floor((CDbl(obj.Location.X) - CDbl(bandMinX(bandIdx))) / gridSize))))
                        yBin = CInt(Math.Max(Integer.MinValue, Math.Min(Integer.MaxValue, Math.Floor((CDbl(obj.Location.Y) - CDbl(bandMinY(bandIdx))) / gridSize))))
                        zBin = CInt(Math.Max(Integer.MinValue, Math.Min(Integer.MaxValue, Math.Floor((CDbl(obj.Location.Z) - CDbl(bandMinZ(bandIdx))) / gridSize))))
                    Catch ex As OverflowException
                        xBin = 0 : yBin = 0 : zBin = 0
                    End Try
                    Dim cell = Tuple.Create(xBin, yBin, zBin)
                    If Not gridDict.ContainsKey(cell) Then gridDict.Add(cell, New List(Of Integer))
                    gridDict(cell).Add(idx)
                Next

                ' Randomize cells and pick one per cell, up to keepPerBand(bandIdx)
                Dim cellKeys As List(Of Tuple(Of Integer, Integer, Integer)) = New List(Of Tuple(Of Integer, Integer, Integer))(gridDict.Keys)
                Dim shuffledCells As New List(Of Tuple(Of Integer, Integer, Integer))
                While cellKeys.Count > 0
                    Dim ci As Integer = rnd.Next(cellKeys.Count)
                    shuffledCells.Add(cellKeys(ci))
                    cellKeys.RemoveAt(ci)
                End While
                Dim bandKeep As Integer = keepPerBand(bandIdx)
                Dim added As Integer = 0
                For Each cell In shuffledCells
                    If added >= bandKeep Then Exit For
                    keepSet.Add(gridDict(cell)(0))
                    added += 1
                Next
                ' If not enough, fill randomly from remaining in band
                If added < bandKeep Then
                    Dim notKept As New List(Of Integer)
                    For Each idx In bandKeys
                        If Not keepSet.Contains(idx) Then notKept.Add(idx)
                    Next
                    While added < bandKeep AndAlso notKept.Count > 0
                        Dim idx2 As Integer = rnd.Next(notKept.Count)
                        keepSet.Add(notKept(idx2))
                        notKept.RemoveAt(idx2)
                        added += 1
                    End While
                End If
            End If
        Next

        For i = 0 To keys.Length - 1
            Dim k As Integer = keys(i)
            If Not keepSet.Contains(k) Then
                Dim removed As MyObject = Nothing
                sourceDict.TryRemove(k, removed)
                ' Do not add to destDict
            End If
        Next

    End Sub











    Public uniqId As Integer = 0
    Function GetNextUniqId() As Integer
        Return Interlocked.Increment(uniqId)
    End Function

    Public Function GetMyObjectByStructureId(structureId As Integer) As MyObject
        Dim obj As MyObject = Nothing
        objectDictionary.TryGetValue(structureId, obj)
        Return obj
    End Function


    Function CalculateFastDistance(point1 As (Integer, Integer, Integer), point2 As (Integer, Integer, Integer)) As Single
        Dim v1 As New Vector3(point1.Item1, point1.Item2, point1.Item3)
        Dim v2 As New Vector3(point2.Item1, point2.Item2, point2.Item3)
        Dim diff As Vector3 = Vector3.Subtract(v2, v1)
        Return diff.Length()
    End Function














    Public Sub HandleMCCommand(apiImpl As ICurrentApi,
                           pluginManager As PluginManager,
                           marginMgr As Margins.MarginManager)

        Dim pluginOrKeyword As String = Nothing

        If Not IniManager.TryGetPluginForCommand(mccommand, pluginOrKeyword) Then
            ' Unknown command; user hasn't defined it in [Commands]
            Exit Sub
        End If

        Select Case pluginOrKeyword
            Case "_builtin_list_plugins"
                pluginManager.ListAvailablePlugins()
                Exit Sub

            Case "_builtin_list_margins"
                PrintAllMargins(marginMgr)
                Exit Sub

                'Case "_builtin_run_cube"
                'pluginManager.RunPluginByNameOnThread("Satellite Cubes Plugin", apiImpl)

                'Thread.Sleep(500)
                'Console.WriteLine(objectDictionary.Count)
                'ThinEvenSpatiallyAdaptiveAuto(objectDictionary, Nothing, 30000, userCoordinates, 200, 20, 1.5)
                'Console.WriteLine(objectDictionary.Count)
                'Console.WriteLine("ini builtin")

            Case Else
                ' Treat as plugin name (must match PluginMetadata.Name)
                pluginManager.RunPluginByNameOnThread(pluginOrKeyword, apiImpl)
        End Select
    End Sub


    'Public Sub HandleMCCommand_2_(apiImpl As ICurrentApi,
    '                       pluginManager As PluginManager,
    '                       marginMgr As Margins.MarginManager)

    '    Dim pluginOrKeyword As String = Nothing

    '    If Not CommandConfig.TryGetPluginForCommand(mccommand, pluginOrKeyword) Then
    '        ' Unknown command; user hasn't defined it in [Commands]
    '        Exit Sub
    '    End If

    '    Select Case pluginOrKeyword
    '        Case "_builtin_list_plugins"
    '            pluginManager.ListAvailablePlugins()
    '            Exit Sub

    '        Case "_builtin_list_margins"
    '            PrintAllMargins(marginMgr)
    '            Exit Sub

    '        Case "_builtin_run_cube"
    '            pluginManager.RunPluginByNameOnThread("Satellite Cubes Plugin", apiImpl)

    '            Thread.Sleep(500)
    '            ' Example: Keep more up close, with a strong bias (1.5), 10 bands, keep all within 500 units
    '            Console.WriteLine(objectDictionary.Count)
    '            ThinEvenSpatiallyAdaptiveAuto(objectDictionary, thinnedDict, 35000, userCoordinates, 200, 20, 20)
    '            Console.WriteLine(objectDictionary.Count)
    '            Console.WriteLine("ini builtin")

    '        Case Else
    '            ' Treat as plugin name (must match PluginMetadata.Name)
    '            pluginManager.RunPluginByNameOnThread(pluginOrKeyword, apiImpl)
    '    End Select
    'End Sub


    ''Sub HandleMCCommand(zone As SpatialZone, apiImpl As ICurrentApi, pluginManager As PluginManager, marginMgr As Margins.MarginManager)
    'Sub HandleMCCommand_old_(apiImpl As ICurrentApi, pluginManager As PluginManager, marginMgr As Margins.MarginManager)
    '    Select Case mccommand
    '        Case "dummy data"
    '            ' Do nothing
    '            'Console.WriteLine(mccommand)
    '        Case "list plugins"
    '            pluginManager.ListAvailablePlugins()
    '        Case "events"
    '            pluginManager.RunPluginByNameOnThread("Event Aggregator", apiImpl)
    '            'pluginManager.RunAllPluginsOnThreads(apiImpl)

    '        Case "menu system"
    '            pluginManager.RunPluginByNameOnThread("Menu System", apiImpl)
    '        Case "test menu"
    '            pluginManager.RunPluginByNameOnThread("Menu System Consumer", apiImpl)

    '        Case "run cube"
    '            pluginManager.RunPluginByNameOnThread("Satellite Cubes Plugin", apiImpl)

    '            Thread.Sleep(500)
    '            ' Example: Keep more up close, with a strong bias (1.5), 10 bands, keep all within 500 units
    '            Console.WriteLine(objectDictionary.Count)
    '            ThinEvenSpatiallyAdaptiveAuto(objectDictionary, thinnedDict, 35000, userCoordinates, 200, 20, 20)
    '            Console.WriteLine(objectDictionary.Count)

    '        Case "test mouseover"
    '            pluginManager.RunPluginByNameOnThread("Spatial Zone Mouse Test", apiImpl)
    '        Case "points"
    '            pluginManager.RunPluginByNameOnThread("Hollow Shell Generator", apiImpl)

    '        Case "list margins"
    '            PrintAllMargins(marginMgr)
    '            'Case "clearsz"
    '            '    Console.WriteLine(mccommand)
    '            '    zone.RemoveAllZoneObjects()
    '            '    mccommand = "dummy data"
    '            'Case "north set"
    '            '    zone.SwapMargenSetNorth()
    '            '    mccommand = "dummy data"
    '            'Case "east set"
    '            '    zone.SwapMargenSetEast()
    '            '    mccommand = "dummy data"
    '            'Case "south set"
    '            '    zone.SwapMargenSetSouth()
    '            '    mccommand = "dummy data"
    '            'Case "west set"
    '            '    zone.SwapMargenSetWest()
    '            '    mccommand = "dummy data"
    '            'Case "top set"
    '            '    zone.SwapMargenSetTop()
    '            '    mccommand = "dummy data"
    '            'Case "bottom set"
    '            '    zone.SwapMargenSetBottom()
    '            '    mccommand = "dummy data"
    '            'Case "move margin"
    '            '    marginMgr.MarginJump("NorthSetRight", PanelType.NorthPanel, Nothing, 160)
    '            '    zone.SwapMargenSetNorth()
    '            '    mccommand = "dummy data"
    '            'Case "move margin2"
    '            '    marginMgr.MarginJump("NorthSetRight", PanelType.NorthPanel, Nothing, 100)
    '            '    zone.SwapMargenSetNorth()
    '            '    mccommand = "dummy data"
    '        Case Else
    '            ' You can add more commands here in the future
    '    End Select
    'End Sub
















    Public observerVectorData As New Dictionary(Of String, Object) From {
    {"Vector", Nothing},
    {"Magnitude", Nothing},
    {"UnitVector", Nothing},
    {"FromPoint", Nothing},
    {"ToPoint", Nothing}}
    ' Method to compute and store all vector info
    Sub UpdateObserverVectorData(fromPoint As (Integer, Integer, Integer), toPointStr As String)
        If String.IsNullOrWhiteSpace(toPointStr) Then
            observerVectorData("Vector") = Nothing
            observerVectorData("Magnitude") = Nothing
            observerVectorData("UnitVector") = Nothing
            observerVectorData("FromPoint") = Nothing
            observerVectorData("ToPoint") = Nothing
            Exit Sub
        End If
        Dim parts = toPointStr.Split(","c)
        If parts.Length <> 3 Then Exit Sub
        Dim toX, toY, toZ As Double
        If Not Double.TryParse(parts(0), Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture, toX) Then Exit Sub
        If Not Double.TryParse(parts(1), Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture, toY) Then Exit Sub
        If Not Double.TryParse(parts(2), Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture, toZ) Then Exit Sub

        Dim toInt = (CInt(Math.Round(toX)), CInt(Math.Round(toY)), CInt(Math.Round(toZ)))
        Dim vector = New Vector3D(toX - fromPoint.Item1, toY - fromPoint.Item2, toZ - fromPoint.Item3)
        Dim magTuple = vector.ToTuple()
        Dim magnitude As Double = Math.Sqrt(magTuple.x * magTuple.x + magTuple.y * magTuple.y + magTuple.z * magTuple.z)
        Dim unitVector As Vector3D = If(magnitude > 0, New Vector3D(magTuple.x / magnitude, magTuple.y / magnitude, magTuple.z / magnitude), New Vector3D(0, 0, 0))

        observerVectorData("Vector") = vector
        observerVectorData("Magnitude") = magnitude
        observerVectorData("UnitVector") = unitVector
        observerVectorData("FromPoint") = fromPoint
        observerVectorData("ToPoint") = toInt
    End Sub


    Class Vector3D
        Private ReadOnly x As Double
        Private ReadOnly y As Double
        Private ReadOnly z As Double
        Sub New(nx As Double, ny As Double, nz As Double)
            x = nx
            y = ny
            z = nz
        End Sub
        Public Function Dot(rhs As Vector3D) As Double
            Return x * rhs.x + y * rhs.y + z * rhs.z
        End Function
        Public Shared Operator +(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x + b.x, a.y + b.y, a.z + b.z)
        End Operator
        Public Shared Operator -(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x - b.x, a.y - b.y, a.z - b.z)
        End Operator
        Public Shared Operator *(ByVal a As Vector3D, ByVal b As Double) As Vector3D
            Return New Vector3D(a.x * b, a.y * b, a.z * b)
        End Operator
        Public Overrides Function ToString() As String
            Return String.Format("{0:F}, {1:F}, {2:F}", x, y, z)
        End Function
        Public Function ToIntTuple() As (Integer, Integer, Integer)
            Return (CInt(Fix(x)), CInt(Fix(y)), CInt(Fix(z)))
        End Function
        Public Function ToTuple() As (x As Double, y As Double, z As Double)
            Return (Math.Round(x, 2), Math.Round(y, 2), Math.Round(z, 2))
        End Function
    End Class

    Public Class PanelBounds
        Private ReadOnly _precalculatedBounds As Dictionary(Of PanelType, (Integer, Integer, Integer, Integer, Integer, Integer))

        Public Sub New()
            _precalculatedBounds = New Dictionary(Of PanelType, (Integer, Integer, Integer, Integer, Integer, Integer))()
            For Each panel In panelData.panelsArray
                _precalculatedBounds.Add(panel.PanelType, CalculateMinMaxBounds(panel.Item2, panel.Item3))
            Next
        End Sub

        Private Function CalculateMinMaxBounds(corner1 As (Integer, Integer, Integer), corner2 As (Integer, Integer, Integer)) As (Integer, Integer, Integer, Integer, Integer, Integer)
            Dim minX As Integer = Math.Min(corner1.Item1, corner2.Item1)
            Dim maxX As Integer = Math.Max(corner1.Item1, corner2.Item1)
            Dim minY As Integer = Math.Min(corner1.Item2, corner2.Item2)
            Dim maxY As Integer = Math.Max(corner1.Item2, corner2.Item2)
            Dim minZ As Integer = Math.Min(corner1.Item3, corner2.Item3)
            Dim maxZ As Integer = Math.Max(corner1.Item3, corner2.Item3)
            Return (minX, maxX, minY, maxY, minZ, maxZ)
        End Function

        Public Function IsPointWithinPanel(panelType As PanelType, point As (Integer, Integer, Integer)) As Boolean
            If _precalculatedBounds.ContainsKey(panelType) Then
                Dim bounds = _precalculatedBounds(panelType)
                Return (point.Item1 >= bounds.Item1) AndAlso (point.Item1 <= bounds.Item2) AndAlso (point.Item2 >= bounds.Item3) AndAlso (point.Item2 <= bounds.Item4) AndAlso (point.Item3 >= bounds.Item5) AndAlso (point.Item3 <= bounds.Item6)
            Else
                Throw New ArgumentException($"Invalid panel type: {panelType}")
            End If
        End Function
    End Class




    ' Bresenham's 3D line algorithm: returns all integer points between (x0,y0,z0) and (x1,y1,z1)
    Function Bresenham3D(startX As Integer, startY As Integer, startZ As Integer, endX As Integer, endY As Integer, endZ As Integer) As List(Of (Integer, Integer, Integer))
        Dim allPoints As New List(Of (Integer, Integer, Integer))
        Dim distanceToTravelX = Math.Abs(endX - startX)
        Dim distanceToTravelY = Math.Abs(endY - startY)
        Dim distanceToTravelZ = Math.Abs(endZ - startZ)
        Dim directionX = If(endX > startX, 1, -1)
        Dim directionY = If(endY > startY, 1, -1)
        Dim directionZ = If(endZ > startZ, 1, -1)
        Dim currentX = startX
        Dim currentY = startY
        Dim currentZ = startZ
        If distanceToTravelX >= distanceToTravelY AndAlso distanceToTravelX >= distanceToTravelZ Then
            Dim stepsNeededToCatchUpY = 2 * distanceToTravelY - distanceToTravelX
            Dim stepsNeededToCatchUpZ = 2 * distanceToTravelZ - distanceToTravelX
            For i = 0 To distanceToTravelX
                allPoints.Add((currentX, currentY, currentZ))
                If stepsNeededToCatchUpY >= 0 Then
                    currentY += directionY
                    stepsNeededToCatchUpY -= 2 * distanceToTravelX
                End If
                If stepsNeededToCatchUpZ >= 0 Then
                    currentZ += directionZ
                    stepsNeededToCatchUpZ -= 2 * distanceToTravelX
                End If
                stepsNeededToCatchUpY += 2 * distanceToTravelY
                stepsNeededToCatchUpZ += 2 * distanceToTravelZ
                currentX += directionX
            Next
        ElseIf distanceToTravelY >= distanceToTravelX AndAlso distanceToTravelY >= distanceToTravelZ Then
            Dim stepsNeededToCatchUpX = 2 * distanceToTravelX - distanceToTravelY
            Dim stepsNeededToCatchUpZ = 2 * distanceToTravelZ - distanceToTravelY
            For i = 0 To distanceToTravelY
                allPoints.Add((currentX, currentY, currentZ))
                If stepsNeededToCatchUpX >= 0 Then
                    currentX += directionX
                    stepsNeededToCatchUpX -= 2 * distanceToTravelY
                End If
                If stepsNeededToCatchUpZ >= 0 Then
                    currentZ += directionZ
                    stepsNeededToCatchUpZ -= 2 * distanceToTravelY
                End If
                stepsNeededToCatchUpX += 2 * distanceToTravelX
                stepsNeededToCatchUpZ += 2 * distanceToTravelZ
                currentY += directionY
            Next

        Else
            Dim stepsNeededToCatchUpY = 2 * distanceToTravelY - distanceToTravelZ
            Dim stepsNeededToCatchUpX = 2 * distanceToTravelX - distanceToTravelZ
            For i = 0 To distanceToTravelZ
                allPoints.Add((currentX, currentY, currentZ))
                If stepsNeededToCatchUpY >= 0 Then
                    currentY += directionY
                    stepsNeededToCatchUpY -= 2 * distanceToTravelZ
                End If
                If stepsNeededToCatchUpX >= 0 Then
                    currentX += directionX
                    stepsNeededToCatchUpX -= 2 * distanceToTravelZ
                End If
                stepsNeededToCatchUpY += 2 * distanceToTravelY
                stepsNeededToCatchUpX += 2 * distanceToTravelX
                currentZ += directionZ
            Next
        End If

        Return allPoints
    End Function


#Region "Collision"




    ' Ensure all setIds that exist in triangleGroups are tracked in allSetIds and triangleSetSortScores
    Private Sub AddAllSetIdsFromTriangleGroups()
        For Each setId In triangleGroups.Keys
            allSetIds.TryAdd(setId, 0)
            triangleSetSortScores.TryAdd(setId, 0)
        Next
    End Sub

    ' Called once per frame, BEFORE Parallel.ForEach over objects.
    ' 1) Ensure all setIds are registered.
    ' 2) Apply per-frame decay to triangleSetSortScores (clamped at 0).
    ' 3) Build a sorted list of setIds into pregeneratedSortedSetIds using reusable buffers.
    Public Sub PrepareSortedSetIdsForFrame()
        ' Step 1: ensure all sets are known (if triangles added between frames)
        AddAllSetIdsFromTriangleGroups()

        ' Step 2: apply decay and clamp at 0
        Dim setIdsSnapshot As New List(Of Integer)(allSetIds.Keys)
        For Each setId As Integer In setIdsSnapshot
            Dim oldVal As Integer
            If triangleSetSortScores.TryGetValue(setId, oldVal) Then
                Dim newVal As Integer = oldVal - SCORE_DECAY_PER_FRAME
                If newVal < 0 Then
                    newVal = 0
                End If
                triangleSetSortScores(setId) = newVal
            Else
                triangleSetSortScores.TryAdd(setId, 0)
            End If
        Next

        ' Step 3: prepare frame-local sorted list (reusing buffers)
        sortScoresBuffer.Clear()
        sortedSetIdsBuffer.Clear()

        ' Snapshot scores into buffer
        For Each kvp As KeyValuePair(Of Integer, Integer) In triangleSetSortScores
            sortScoresBuffer(kvp.Key) = kvp.Value
        Next

        ' Push sets that did NOT block this frame down to the bottom
        Dim lowestScore As Integer = Integer.MinValue
        For Each setId As Integer In allSetIds.Keys
            If Not hotSetIds.ContainsKey(setId) Then
                sortScoresBuffer(setId) = lowestScore
            End If
        Next





        ' Debug: snapshot the exact scores used for sorting this frame
        sortScoresDebugSnapshot.Clear()
        For Each kvp As KeyValuePair(Of Integer, Integer) In sortScoresBuffer
            sortScoresDebugSnapshot(kvp.Key) = kvp.Value
        Next






        ' Sort keys by score descending into reusable list
        For Each kvp As KeyValuePair(Of Integer, Integer) In sortScoresBuffer
            sortedSetIdsBuffer.Add(kvp.Key)
        Next

        sortedSetIdsBuffer.Sort(
        Function(a As Integer, b As Integer)
            Dim sa As Integer = sortScoresBuffer(a)
            Dim sb As Integer = sortScoresBuffer(b)
            ' descending
            Return sb.CompareTo(sa)
        End Function)

        ' Assign to pregeneratedSortedSetIds (this list is used read-only during the frame)
        pregeneratedSortedSetIds = New List(Of Integer)(sortedSetIdsBuffer)
    End Sub

    ' Call this at the very start of each frame, on the main thread,
    ' BEFORE starting Parallel.ForEach. It clears any leftover hot flags.
    Public Sub BeginFrameScoring()
        hotSetIds.Clear()
    End Sub

    Public Function GetSortedSetIdsForFrame() As List(Of Integer)
        Return pregeneratedSortedSetIds
    End Function

    ' Clear per-frame hot flags AFTER frame processing is done.
    ' Call this ONCE per frame, after Parallel.ForEach.
    Public Sub EndOfFrameCleanup()
        hotSetIds.Clear()
    End Sub


    ' Bump a set's score when it actually blocks a ray (global, no lambdas).
    Private Sub BumpTriangleSetScore_Global(setId As Integer)
        Dim current As Integer = 0
        If triangleSetSortScores.TryGetValue(setId, current) Then
            Dim newVal As Integer = current + SCORE_BUMP_ON_BLOCK
            triangleSetSortScores(setId) = newVal
        Else
            triangleSetSortScores.TryAdd(setId, SCORE_BUMP_ON_BLOCK)
        End If

        hotSetIds.TryAdd(setId, 0)


        ' Debug: count how many times this set blocked in this frame
        SyncLock perSetBlocksThisFrame
            Dim c As Integer = 0
            If perSetBlocksThisFrame.TryGetValue(setId, c) Then
                perSetBlocksThisFrame(setId) = c + 1
            Else
                perSetBlocksThisFrame(setId) = 1
            End If
        End SyncLock

        ' Debug: count total blocks this frame
        Interlocked.Increment(blocksThisFrame)

    End Sub



    Public Function IsRayObscuredByAnyTriangle(
    observer As (Double, Double, Double),
    target As (Double, Double, Double)) As (Blocked As Boolean, BlockedSetId As Integer)


        ' Count how many rays we cast this frame (debug only)
        Interlocked.Increment(raysThisFrame)


        ' Compute ray direction ONCE per call (per object)
        Dim rayDir As (Double, Double, Double) = (
        target.Item1 - observer.Item1,
        target.Item2 - observer.Item2,
        target.Item3 - observer.Item3
    )

        ' Decide set order for this frame
        Dim setsToCheck As IEnumerable(Of Integer)
        Dim sortedSetIdsList As List(Of Integer) = GetSortedSetIdsForFrame()
        If sortedSetIdsList IsNot Nothing AndAlso sortedSetIdsList.Count > 0 Then
            setsToCheck = sortedSetIdsList
        Else
            setsToCheck = triangleGroups.Keys
        End If

        For Each setId As Integer In setsToCheck
            Dim triangleIds As ConcurrentBag(Of Integer) = Nothing
            If triangleGroups.TryGetValue(setId, triangleIds) Then
                For Each triangleId As Integer In triangleIds
                    Dim triangle As Triangle
                    If trianglesById.TryGetValue(triangleId, triangle) Then
                        ' --- AABB SHORT-CIRCUIT ---
                        ' Compute per-frame triangle AABB (triangles move each frame)
                        Dim minX As Double = Math.Min(triangle.A.Item1, Math.Min(triangle.B.Item1, triangle.C.Item1))
                        Dim minY As Double = Math.Min(triangle.A.Item2, Math.Min(triangle.B.Item2, triangle.C.Item2))
                        Dim minZ As Double = Math.Min(triangle.A.Item3, Math.Min(triangle.B.Item3, triangle.C.Item3))
                        Dim maxX As Double = Math.Max(triangle.A.Item1, Math.Max(triangle.B.Item1, triangle.C.Item1))
                        Dim maxY As Double = Math.Max(triangle.A.Item2, Math.Max(triangle.B.Item2, triangle.C.Item2))
                        Dim maxZ As Double = Math.Max(triangle.A.Item3, Math.Max(triangle.B.Item3, triangle.C.Item3))

                        If Not RayIntersectsAABB(observer, rayDir, (minX, minY, minZ), (maxX, maxY, maxZ)) Then
                            ' Ray can't reach this triangle; skip precise intersection math
                            Continue For
                        End If

                        ' Precise ray/triangle intersection
                        If RayIntersectsTriangle(observer, target, triangle) Then
                            ' Bump score since THIS set actually blocked the ray
                            BumpTriangleSetScore_Global(setId)
                            Return (True, setId)
                        End If
                    End If
                Next
            End If
        Next

        Return (False, -1)
    End Function

    Public Function RayIntersectsTriangle(observer As (Double, Double, Double), target As (Double, Double, Double), tri As Triangle) As Boolean
        Dim orig = observer
        Dim dir = (target.Item1 - observer.Item1, target.Item2 - observer.Item2, target.Item3 - observer.Item3)
        Dim len = Math.Sqrt(dir.Item1 * dir.Item1 + dir.Item2 * dir.Item2 + dir.Item3 * dir.Item3)
        If len = 0 Then Return False
        Dim dirNorm = (dir.Item1 / len, dir.Item2 / len, dir.Item3 / len)
        Dim v0 = tri.A
        Dim v1 = tri.B
        Dim v2 = tri.C
        Dim eps = 0.00001

        ' Edges
        Dim edge1 = (v1.Item1 - v0.Item1, v1.Item2 - v0.Item2, v1.Item3 - v0.Item3)
        Dim edge2 = (v2.Item1 - v0.Item1, v2.Item2 - v0.Item2, v2.Item3 - v0.Item3)
        Dim h = Cross(dirNorm, edge2)
        Dim a = Dot(edge1, h)
        If Math.Abs(a) < eps Then Return False ' Parallel
        Dim f = 1.0 / a
        Dim s = (orig.Item1 - v0.Item1, orig.Item2 - v0.Item2, orig.Item3 - v0.Item3)
        Dim u = f * Dot(s, h)
        If u < 0.0 Or u > 1.0 Then Return False
        Dim q = Cross(s, edge1)
        Dim v = f * Dot(dirNorm, q)
        If v < 0.0 Or (u + v) > 1.0 Then Return False
        Dim t = f * Dot(edge2, q)
        If t > eps And t < len - eps Then Return True
        Return False
    End Function


    ' Fast Ray-AABB intersection using the slab method
    Private Function RayIntersectsAABB(rayOrigin As (Double, Double, Double), rayDir As (Double, Double, Double), min As (Double, Double, Double), max As (Double, Double, Double)) As Boolean
        Dim tmin As Double = Double.NegativeInfinity
        Dim tmax As Double = Double.PositiveInfinity

        For i As Integer = 0 To 2
            Dim origin As Double, dir As Double, bmin As Double, bmax As Double
            If i = 0 Then
                origin = rayOrigin.Item1 : dir = rayDir.Item1 : bmin = min.Item1 : bmax = max.Item1
            ElseIf i = 1 Then
                origin = rayOrigin.Item2 : dir = rayDir.Item2 : bmin = min.Item2 : bmax = max.Item2
            Else
                origin = rayOrigin.Item3 : dir = rayDir.Item3 : bmin = min.Item3 : bmax = max.Item3
            End If

            If Math.Abs(dir) < 0.00000001 Then
                If origin < bmin OrElse origin > bmax Then Return False
            Else
                Dim invD = 1.0 / dir
                Dim t0 = (bmin - origin) * invD
                Dim t1 = (bmax - origin) * invD
                If t0 > t1 Then
                    Dim tmp = t0 : t0 = t1 : t1 = tmp
                End If
                tmin = Math.Max(tmin, t0)
                tmax = Math.Min(tmax, t1)
                If tmax < tmin Then Return False
            End If
        Next
        Return True
    End Function


    Private Function Dot(a As (Double, Double, Double), b As (Double, Double, Double)) As Double
        Return a.Item1 * b.Item1 + a.Item2 * b.Item2 + a.Item3 * b.Item3
    End Function
    Private Function Cross(a As (Double, Double, Double), b As (Double, Double, Double)) As (Double, Double, Double)
        Return (
            a.Item2 * b.Item3 - a.Item3 * b.Item2,
            a.Item3 * b.Item1 - a.Item1 * b.Item3,
            a.Item1 * b.Item2 - a.Item2 * b.Item1)
    End Function


    Public Structure Triangle
        Public A As (Double, Double, Double)
        Public B As (Double, Double, Double)
        Public C As (Double, Double, Double)
        Public TriangleSetId As Integer
        Public TriangleId As Integer

        Public Sub New(ax As Double, ay As Double, az As Double,
                   bx As Double, by As Double, bz As Double,
                   cx As Double, cy As Double, cz As Double,
                   setId As Integer, triangleId As Integer)
            A = (ax, ay, az)
            B = (bx, by, bz)
            C = (cx, cy, cz)
            TriangleSetId = setId
            Me.TriangleId = triangleId
        End Sub
    End Structure




    Public Sub InitializeSortedSetIds()
        triangleSetSortScores.Clear()
        For Each setId In triangleGroups.Keys
            triangleSetSortScores.TryAdd(setId, 0)
        Next
    End Sub




    Public Sub RemoveAllTrianglesInSet(setId As Integer)
        Dim bag As ConcurrentBag(Of Integer) = Nothing
        If triangleGroups.TryRemove(setId, bag) Then
            For Each triangleId In bag
                trianglesById.TryRemove(triangleId, Nothing)
            Next
        End If

        Dim dummyByte As Byte
        Dim dummyInt As Integer

        sortedSetIds.TryRemove(setId, dummyByte)
        allSetIds.TryRemove(setId, dummyByte)
        triangleSetSortScores.TryRemove(setId, dummyInt)
    End Sub


    Public Function AddTriangle(
    ax As Double, ay As Double, az As Double,
    bx As Double, by As Double, bz As Double,
    cx As Double, cy As Double, cz As Double,
    setId As Integer) As Integer

        Dim triangleId As Integer = GetNextUniqId()
        Dim t As New Triangle(ax, ay, az, bx, by, bz, cx, cy, cz, setId, triangleId)
        trianglesById.TryAdd(triangleId, t)

        Dim bag As ConcurrentBag(Of Integer) = Nothing
        If Not triangleGroups.TryGetValue(setId, bag) Then
            bag = New ConcurrentBag(Of Integer)()
            triangleGroups.TryAdd(setId, bag)
        End If
        bag.Add(triangleId)

        ' Track this setId in the hot-bucket system
        allSetIds.TryAdd(setId, 0)
        triangleSetSortScores.TryAdd(setId, 0)
        sortedSetIds.TryAdd(setId, 0)

        Return triangleId
    End Function





    ' Debug snapshot of sort scores actually used for the current frame's order
    Private sortScoresDebugSnapshot As New Dictionary(Of Integer, Integer)()
    Private hudFrameCounter As Long = 0

    ' --- Debug / instrumentation for triangle sort per frame ---
    Private raysThisFrame As Long = 0
    Private blocksThisFrame As Long = 0
    Private perSetBlocksThisFrame As New Dictionary(Of Integer, Integer)()








    Private Sub RenderTriangleSortDebugOverlay()
        ' -----------------------------------------------------------------------
        ' TRIANGLE SORT DEBUG HUD (lightweight)
        '  - Updates only every N frames to limit overhead.
        '  - Shows just enough to verify the collider / sorter is active.
        ' -----------------------------------------------------------------------

        Const HUD_INTERVAL As Integer = 30 ' adjust as needed
        hudFrameCounter += 1
        If (hudFrameCounter Mod HUD_INTERVAL) <> 0 Then
            Return
        End If

        Const startLeft As Integer = 0
        Const startTop As Integer = 0
        Const topN As Integer = 5

        Try
            ' Snapshot core stats (minimal locking/alloc)
            Dim setCount As Integer = allSetIds.Count
            Dim triangleCount As Integer = trianglesById.Count

            Dim sortedList As List(Of Integer) = GetSortedSetIdsForFrame()
            Dim sortedCount As Integer = If(sortedList Is Nothing, 0, sortedList.Count)

            Dim totalRays As Long = raysThisFrame
            Dim totalBlocks As Long = blocksThisFrame

            Dim lines As New List(Of String)()

            ' Global info (compact but clear)
            lines.Add($"Frame index (processed so far)        : {hudFrameCounter}")
            lines.Add($"Triangle sets currently registered    : {setCount}")
            lines.Add($"Triangles currently registered         : {triangleCount}")
            lines.Add($"Sets included in current sort order    : {sortedCount}")
            lines.Add($"Rays cast during this frame            : {totalRays}")
            lines.Add($"Ray blocks detected this frame         : {totalBlocks}")

            If setCount > 0 Then
                Dim avgTrisPerSet As Double = triangleCount / Math.Max(1.0, CDbl(setCount))
                lines.Add($"Average triangles per set              : {avgTrisPerSet:F2}")
            Else
                lines.Add("Average triangles per set              : n/a")
            End If

            ' Top N sets in current frame order (lightweight view)
            If sortedList IsNot Nothing AndAlso sortedList.Count > 0 Then
                Dim limit As Integer = Math.Min(topN, sortedList.Count)
                lines.Add("Top sets in current sort order (per set line):")
                lines.Add("  Format: SetId | RawScore (accumulated) | TrianglesInSet")

                For i As Integer = 0 To limit - 1
                    Dim setId As Integer = sortedList(i)

                    ' RawScore: live underlying score after this frame's bumps
                    Dim rawScore As Integer = 0
                    triangleSetSortScores.TryGetValue(setId, rawScore)

                    ' Triangle count in set
                    Dim triCountInSet As Integer = 0
                    Dim bag As ConcurrentBag(Of Integer) = Nothing
                    If triangleGroups.TryGetValue(setId, bag) AndAlso bag IsNot Nothing Then
                        triCountInSet = bag.Count
                    End If

                    lines.Add(
                        $"  Set {setId}: RawScore={rawScore} | Tris={triCountInSet}"
                    )
                Next
            Else
                lines.Add("No sorted set order is available for this frame.")
            End If

            ' Pad with blank lines to overwrite any old, longer output
            lines.Add("") : lines.Add("") : lines.Add("")

            ' --- Render to console in a fixed area ---
            Dim originalLeft As Integer
            Dim originalTop As Integer

            Try
                originalLeft = Console.CursorLeft
                originalTop = Console.CursorTop
            Catch
                Return ' No console, skip HUD
            End Try

            Try
                Console.CursorVisible = False
            Catch
            End Try

            For i As Integer = 0 To lines.Count - 1
                Dim lineText As String = lines(i)

                Dim targetTop As Integer = startTop + i
                If targetTop >= Console.BufferHeight Then Exit For

                Console.SetCursorPosition(startLeft, targetTop)

                Dim maxWidth As Integer = Console.WindowWidth - startLeft - 1
                If maxWidth < 0 Then maxWidth = 0

                Dim padded As String
                If lineText.Length > maxWidth AndAlso maxWidth > 0 Then
                    padded = lineText.Substring(0, maxWidth)
                ElseIf maxWidth > 0 Then
                    padded = lineText.PadRight(maxWidth)
                Else
                    padded = lineText
                End If

                Console.Write(padded)
            Next

            Try
                Console.SetCursorPosition(originalLeft, originalTop)
            Catch
            End Try

        Catch
            ' HUD is debug-only; swallow console issues.
        End Try
    End Sub






#End Region






End Module


Public Module PanelHelpers
    ' Returns a tuple: (intersectionsArr, intersectionsValidMask)
    ' intersectionsArr is an array (PanelTypeCount) of Vector3D, intersectionsValidMask as bitmask of valid panels hit.
    Public Function GetHolodecPanelIntersections(
        observer As (Integer, Integer, Integer),
        obj As (Integer, Integer, Integer),
        panelData As PanelDataManager,
        PanelTypeCount As Integer
    ) As (Vector3D(), Integer)
        Dim ox = observer.Item1
        Dim oy = observer.Item2
        Dim oz = observer.Item3
        Dim px = obj.Item1
        Dim py = obj.Item2
        Dim pz = obj.Item3

        Dim zNorth = panelData.North_a.Item3
        Dim zSouth = panelData.South_a.Item3
        Dim xEast = panelData.East_a.Item1
        Dim xWest = panelData.West_a.Item1
        Dim yTop = panelData.Top_a.Item2
        Dim yBottom = panelData.Bottom_a.Item2

        Dim tHits As New List(Of (PanelType, Double))

        If px <> ox Then
            tHits.Add((PanelType.EastPanel, (xEast - ox) / (px - ox)))
            tHits.Add((PanelType.WestPanel, (xWest - ox) / (px - ox)))
        End If
        If py <> oy Then
            tHits.Add((PanelType.TopPanel, (yTop - oy) / (py - oy)))
            tHits.Add((PanelType.BottomPanel, (yBottom - oy) / (py - oy)))
        End If
        If pz <> oz Then
            tHits.Add((PanelType.NorthPanel, (zNorth - oz) / (pz - oz)))
            tHits.Add((PanelType.SouthPanel, (zSouth - oz) / (pz - oz)))
        End If

        Dim validHits = tHits.Where(Function(hit) hit.Item2 >= 0 AndAlso hit.Item2 <= 1).ToList()
        validHits.Sort(Function(a, b) a.Item2.CompareTo(b.Item2))

        If validHits.Count = 0 Then
            Return (New Vector3D(PanelTypeCount - 1) {}, 0)
        End If

        If validHits.Count > 2 Then
            validHits = {validHits(0), validHits(1)}.ToList()
        End If

        Dim intersectionsArr(PanelTypeCount - 1) As Vector3D
        Dim intersectionsValidMask As Integer = 0

        For Each hit In validHits
            Dim t = hit.Item2
            Dim panelType = hit.Item1
            Dim ix = CInt(Math.Round(ox + t * (px - ox)))
            Dim iy = CInt(Math.Round(oy + t * (py - oy)))
            Dim iz = CInt(Math.Round(oz + t * (pz - oz)))
            intersectionsArr(panelType) = New Vector3D(ix, iy, iz)
            intersectionsValidMask = intersectionsValidMask Or (1 << panelType)
        Next

        Return (intersectionsArr, intersectionsValidMask)
    End Function
End Module




#Region "panels"
' Handles all geometry and panel/corner definitions for the cube


Public Class PanelDataManager

    ' These arrays describe the panels of the cube as tuples of corner coordinates
    Public panelsArray() As (PanelType As PanelType, FirstTuple As (Integer, Integer, Integer), SecondTuple As (Integer, Integer, Integer))
    Public panelNormalsArray() As (PanelType As PanelType, Normal As (Integer, Integer, Integer), SecondTuple As (Integer, Integer, Integer))
    Public panelName_CornerArray() As (PanelCorner As PanelCorner, Corner As (Integer, Integer, Integer))


    Public ReadOnly Property Center As (Integer, Integer, Integer)
        Get
            Return CenterCoordinates
        End Get
    End Property

    Public PanelInfo As New Dictionary(Of PanelType, Dictionary(Of String, Object)) ' Object because we are storing various types. 

    Public TopPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
    Public BottomPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
    Public NorthPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
    Public SouthPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
    Public EastPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
    Public WestPanelGrid As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))

    ' Cube center coordinates
    Private CenterCoordinates As (centerX As Integer, centerY As Integer, centerZ As Integer)
    ' All cube panel corners
    Public North_a As (Integer, Integer, Integer)
    Public North_b As (Integer, Integer, Integer)
    Public North_c As (Integer, Integer, Integer)
    Public North_d As (Integer, Integer, Integer)
    Public South_a As (Integer, Integer, Integer)
    Public South_b As (Integer, Integer, Integer)
    Public South_c As (Integer, Integer, Integer)
    Public South_d As (Integer, Integer, Integer)
    Public West_a As (Integer, Integer, Integer)
    Public West_b As (Integer, Integer, Integer)
    Public West_c As (Integer, Integer, Integer)
    Public West_d As (Integer, Integer, Integer)
    Public East_a As (Integer, Integer, Integer)
    Public East_b As (Integer, Integer, Integer)
    Public East_c As (Integer, Integer, Integer)
    Public East_d As (Integer, Integer, Integer)
    Public Top_a As (Integer, Integer, Integer)
    Public Top_b As (Integer, Integer, Integer)
    Public Top_c As (Integer, Integer, Integer)
    Public Top_d As (Integer, Integer, Integer)
    Public Bottom_a As (Integer, Integer, Integer)
    Public Bottom_b As (Integer, Integer, Integer)
    Public Bottom_c As (Integer, Integer, Integer)
    Public Bottom_d As (Integer, Integer, Integer)


    Public Sub New()
        Dim centerX As Integer = IniManager.GetSectionInt("CubeCenter", "CenterX", -250)
        Dim centerY As Integer = IniManager.GetSectionInt("CubeCenter", "CenterY", 73)
        Dim centerZ As Integer = IniManager.GetSectionInt("CubeCenter", "CenterZ", -78)

        CenterCoordinates = (centerX, centerY, centerZ)
        populateCorners(CenterCoordinates)
        PrecomputeAllPanelGrids()
        PopulatePanelInfo()
    End Sub

    'Public Sub New_old2_()
    '    Dim baseDir As String = AppContext.BaseDirectory
    '    Dim iniPath = Path.Combine(baseDir, "commands.ini")

    '    Dim centerX As Integer = -250 ' these "values" won't get "used" as the program exists if the ini does not exist.
    '    Dim centerY As Integer = 73
    '    Dim centerZ As Integer = -78

    '    If File.Exists(iniPath) Then
    '        Dim inSection As Boolean = False

    '        For Each raw In File.ReadAllLines(iniPath)
    '            Dim line = raw.Trim()

    '            ' Check for section header
    '            If line.StartsWith("[") AndAlso line.EndsWith("]") Then
    '                inSection = line.Equals("[CubeCenter]", StringComparison.OrdinalIgnoreCase)
    '                Continue For
    '            End If

    '            If Not inSection Then Continue For

    '            ' Skip blanks/comments
    '            If line = "" OrElse line.StartsWith(";") OrElse line.StartsWith("#") Then
    '                Continue For
    '            End If

    '            ' Parse Key=Value
    '            Dim eqIndex = line.IndexOf("="c)
    '            If eqIndex > 0 Then
    '                Dim k = line.Substring(0, eqIndex).Trim().ToLower()
    '                Dim v = line.Substring(eqIndex + 1).Trim()

    '                Select Case k
    '                    Case "centerx"
    '                        Integer.TryParse(v, centerX)
    '                    Case "centery"
    '                        Integer.TryParse(v, centerY)
    '                    Case "centerz"
    '                        Integer.TryParse(v, centerZ)
    '                End Select
    '            End If
    '        Next
    '    End If

    '    CenterCoordinates = (centerX, centerY, centerZ)
    '    populateCorners(CenterCoordinates)
    '    PrecomputeAllPanelGrids()
    '    PopulatePanelInfo()
    'End Sub


    'Public Sub New_old_()
    '    ' Prompt user for cube center; used for all panel/corner calculations
    '    Console.Write("Enter the X coordinate of the center (-575 or -250): ")
    '    Dim centerX As Integer = Convert.ToInt32(Console.ReadLine())
    '    Console.Write("Enter the Y coordinate of the center (81 or 73): ")
    '    Dim centerY As Integer = Convert.ToInt32(Console.ReadLine())
    '    Console.Write("Enter the Z coordinate of the center (-512 or -78): ")
    '    Dim centerZ As Integer = Convert.ToInt32(Console.ReadLine())
    '    CenterCoordinates = (centerX, centerY, centerZ)
    '    populateCorners(CenterCoordinates)
    '    PrecomputeAllPanelGrids() ' ^^ call after populateCorners
    '    PopulatePanelInfo() ' ^^ call after PrecomputeAllPanelGrids
    'End Sub

    Private Sub PopulatePanelInfo()
        For Each panel As PanelType In [Enum].GetValues(GetType(PanelType))
            Dim dict As New Dictionary(Of String, Object)()

            dict("PanelName") = panel.ToString()
            dict("FurthestLeftColumn") = GetFurthestLeftColumnIndex(panel)
            dict("FurthestRightColumn") = GetFurthestRightColumnIndex(panel)
            dict("FurthestTopRow") = GetTopmostRowIndex(panel)
            dict("FurthestBottomRow") = GetBottommostRowIndex(panel)
            dict("TotalColumns") = GetFurthestRightColumnIndex(panel) - GetFurthestLeftColumnIndex(panel) + 1
            dict("TotalRows") = GetBottommostRowIndex(panel) - GetTopmostRowIndex(panel) + 1

            ' Cube Face Adjacency Logic:
            ' For the TopPanel and BottomPanel, "above", "below", "left", and "right" are defined
            ' relative to the user standing at the center of that panel:
            ' - On TopPanel, facing NorthPanel: "above" = NorthPanel, "below" = SouthPanel,
            '   "left" = WestPanel, "right" = EastPanel.
            ' - On BottomPanel, facing SouthPanel: "above" = SouthPanel, "below" = NorthPanel,
            '   "left" = WestPanel, "right" = EastPanel.
            ' This convention matches typical 3D graphics orientation, ensuring all panels have
            ' four adjacencies even for top and bottom faces.

            Select Case panel
                Case PanelType.TopPanel
                    dict("AdjacentAbove") = PanelType.NorthPanel
                    dict("AdjacentBelow") = PanelType.SouthPanel
                    dict("AdjacentLeft") = PanelType.WestPanel
                    dict("AdjacentRight") = PanelType.EastPanel
                Case PanelType.BottomPanel
                    dict("AdjacentAbove") = PanelType.SouthPanel
                    dict("AdjacentBelow") = PanelType.NorthPanel
                    dict("AdjacentLeft") = PanelType.WestPanel
                    dict("AdjacentRight") = PanelType.EastPanel
                Case PanelType.NorthPanel
                    dict("AdjacentAbove") = PanelType.TopPanel
                    dict("AdjacentBelow") = PanelType.BottomPanel
                    dict("AdjacentLeft") = PanelType.WestPanel
                    dict("AdjacentRight") = PanelType.EastPanel
                Case PanelType.SouthPanel
                    dict("AdjacentAbove") = PanelType.TopPanel
                    dict("AdjacentBelow") = PanelType.BottomPanel
                    dict("AdjacentLeft") = PanelType.EastPanel
                    dict("AdjacentRight") = PanelType.WestPanel
                Case PanelType.EastPanel
                    dict("AdjacentAbove") = PanelType.TopPanel
                    dict("AdjacentBelow") = PanelType.BottomPanel
                    dict("AdjacentLeft") = PanelType.NorthPanel
                    dict("AdjacentRight") = PanelType.SouthPanel
                Case PanelType.WestPanel
                    dict("AdjacentAbove") = PanelType.TopPanel
                    dict("AdjacentBelow") = PanelType.BottomPanel
                    dict("AdjacentLeft") = PanelType.SouthPanel
                    dict("AdjacentRight") = PanelType.NorthPanel
            End Select

            ' Add a reference to the grid for this panel to the PanelInfo dictionary
            dict("Grid") = GetPanelGrid(panel)

            PanelInfo(panel) = dict
        Next
    End Sub

    Public Sub PrecomputeAllPanelGrids()
        TopPanelGrid = PrecomputePanelGrid(AddressOf TopPanelRowColToXYZ, Top_a, Top_b, Top_d)
        BottomPanelGrid = PrecomputePanelGrid(AddressOf BottomPanelRowColToXYZ, Bottom_a, Bottom_b, Bottom_d)
        NorthPanelGrid = PrecomputePanelGrid(AddressOf NorthPanelRowColToXYZ, North_d, North_a, North_c)
        SouthPanelGrid = PrecomputePanelGrid(AddressOf SouthPanelRowColToXYZ, South_b, South_c, South_a)
        EastPanelGrid = PrecomputePanelGrid(AddressOf EastPanelRowColToXYZ, East_b, East_c, East_a)
        WestPanelGrid = PrecomputePanelGrid(AddressOf WestPanelRowColToXYZ, West_d, West_a, West_c)
    End Sub

    Private Function PrecomputePanelGrid(rowColToXYZ As Func(Of Integer, Integer, (Integer, Integer, Integer)), origin As (Integer, Integer, Integer), rowEnd As (Integer, Integer, Integer), colEnd As (Integer, Integer, Integer)) As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
        Dim grid As New Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))

        ' Compute number of rows and columns, including both start and end positions
        Dim numRows = Math.Abs(rowEnd.Item1 - origin.Item1) + Math.Abs(rowEnd.Item2 - origin.Item2) + Math.Abs(rowEnd.Item3 - origin.Item3) + 1
        Dim numCols = Math.Abs(colEnd.Item1 - origin.Item1) + Math.Abs(colEnd.Item2 - origin.Item2) + Math.Abs(colEnd.Item3 - origin.Item3) + 1

        For row = 1 To numRows
            For col = 1 To numCols
                grid((row, col)) = rowColToXYZ(row, col)
            Next
        Next
        Return grid
    End Function

    Enum Corner
        a
        b
        c
        d
    End Enum

    ' Calculates all panel corners based on cube center and side length
    Sub populateCorners(center As (Integer, Integer, Integer))
        Const halfSideLength As Integer = 121
        Dim A As (Integer, Integer, Integer) = (center.Item1 - halfSideLength, center.Item2, center.Item3 - halfSideLength)
        Dim B As (Integer, Integer, Integer) = (center.Item1 + halfSideLength, center.Item2, center.Item3 - halfSideLength)
        Dim C As (Integer, Integer, Integer) = (center.Item1 + halfSideLength, center.Item2, center.Item3 + halfSideLength)
        Dim D As (Integer, Integer, Integer) = (center.Item1 - halfSideLength, center.Item2, center.Item3 + halfSideLength)
        Dim bottom As New Dictionary(Of Corner, (Integer, Integer, Integer))
        bottom.Add(Corner.a, A)
        bottom.Add(Corner.b, B)
        bottom.Add(Corner.c, C)
        bottom.Add(Corner.d, D)
        Const elevation As Integer = 162
        Dim Top As New Dictionary(Of Corner, (Integer, Integer, Integer))
        Top.Add(Corner.c, (bottom(Corner.b).Item1, bottom(Corner.b).Item2 + elevation, bottom(Corner.b).Item3))
        Top.Add(Corner.d, (bottom(Corner.a).Item1, bottom(Corner.a).Item2 + elevation, bottom(Corner.a).Item3))
        Top.Add(Corner.b, (bottom(Corner.c).Item1, bottom(Corner.c).Item2 + elevation, bottom(Corner.c).Item3))
        Top.Add(Corner.a, (bottom(Corner.d).Item1, bottom(Corner.d).Item2 + elevation, bottom(Corner.d).Item3))
        Dim North As New Dictionary(Of Corner, (Integer, Integer, Integer))
        North.Add(Corner.c, bottom(Corner.b))
        North.Add(Corner.d, bottom(Corner.a))
        North.Add(Corner.b, (North(Corner.c).Item1, North(Corner.c).Item2 + elevation, North(Corner.c).Item3))
        North.Add(Corner.a, (North(Corner.d).Item1, North(Corner.d).Item2 + elevation, North(Corner.d).Item3))
        Dim South As New Dictionary(Of Corner, (Integer, Integer, Integer))
        South.Add(Corner.c, bottom(Corner.d))
        South.Add(Corner.d, bottom(Corner.c))
        South.Add(Corner.b, (South(Corner.c).Item1, South(Corner.c).Item2 + elevation, South(Corner.c).Item3))
        South.Add(Corner.a, (South(Corner.d).Item1, South(Corner.d).Item2 + elevation, South(Corner.d).Item3))
        Dim West As New Dictionary(Of Corner, (Integer, Integer, Integer))
        West.Add(Corner.c, bottom(Corner.a))
        West.Add(Corner.d, bottom(Corner.d))
        West.Add(Corner.b, (West(Corner.c).Item1, West(Corner.c).Item2 + elevation, West(Corner.c).Item3))
        West.Add(Corner.a, (West(Corner.d).Item1, West(Corner.d).Item2 + elevation, West(Corner.d).Item3))
        Dim East As New Dictionary(Of Corner, (Integer, Integer, Integer))
        East.Add(Corner.c, bottom(Corner.c))
        East.Add(Corner.d, bottom(Corner.b))
        East.Add(Corner.b, (East(Corner.c).Item1, East(Corner.c).Item2 + elevation, East(Corner.c).Item3))
        East.Add(Corner.a, (East(Corner.d).Item1, East(Corner.d).Item2 + elevation, East(Corner.d).Item3))
        Top(Corner.a) = (Top(Corner.a).Item1 + 1, Top(Corner.a).Item2, Top(Corner.a).Item3 - 1)
        Top(Corner.b) = (Top(Corner.b).Item1 - 1, Top(Corner.b).Item2, Top(Corner.b).Item3 - 1)
        Top(Corner.c) = (Top(Corner.c).Item1 - 1, Top(Corner.c).Item2, Top(Corner.c).Item3 + 1)
        Top(Corner.d) = (Top(Corner.d).Item1 + 1, Top(Corner.d).Item2, Top(Corner.d).Item3 + 1)
        bottom(Corner.a) = (bottom(Corner.a).Item1 + 1, bottom(Corner.a).Item2, bottom(Corner.a).Item3 + 1)
        bottom(Corner.b) = (bottom(Corner.b).Item1 - 1, bottom(Corner.b).Item2, bottom(Corner.b).Item3 + 1)
        bottom(Corner.c) = (bottom(Corner.c).Item1 - 1, bottom(Corner.c).Item2, bottom(Corner.c).Item3 - 1)
        bottom(Corner.d) = (bottom(Corner.d).Item1 + 1, bottom(Corner.d).Item2, bottom(Corner.d).Item3 - 1)
        West(Corner.a) = (West(Corner.a).Item1, West(Corner.a).Item2 - 1, West(Corner.a).Item3 - 1)
        West(Corner.b) = (West(Corner.b).Item1, West(Corner.b).Item2 - 1, West(Corner.b).Item3 + 1)
        West(Corner.c) = (West(Corner.c).Item1, West(Corner.c).Item2 + 1, West(Corner.c).Item3 + 1)
        West(Corner.d) = (West(Corner.d).Item1, West(Corner.d).Item2 + 1, West(Corner.d).Item3 - 1)
        North(Corner.a) = (North(Corner.a).Item1 + 1, North(Corner.a).Item2 - 1, North(Corner.a).Item3)
        North(Corner.b) = (North(Corner.b).Item1 - 1, North(Corner.b).Item2 - 1, North(Corner.b).Item3)
        North(Corner.c) = (North(Corner.c).Item1 - 1, North(Corner.c).Item2 + 1, North(Corner.c).Item3)
        North(Corner.d) = (North(Corner.d).Item1 + 1, North(Corner.d).Item2 + 1, North(Corner.d).Item3)
        South(Corner.a) = (South(Corner.a).Item1 - 1, South(Corner.a).Item2 - 1, South(Corner.a).Item3)
        South(Corner.b) = (South(Corner.b).Item1 + 1, South(Corner.b).Item2 - 1, South(Corner.b).Item3)
        South(Corner.c) = (South(Corner.c).Item1 + 1, South(Corner.c).Item2 + 1, South(Corner.c).Item3)
        South(Corner.d) = (South(Corner.d).Item1 - 1, South(Corner.d).Item2 + 1, South(Corner.d).Item3)
        East(Corner.a) = (East(Corner.a).Item1, East(Corner.a).Item2 - 1, East(Corner.a).Item3 + 1)
        East(Corner.b) = (East(Corner.b).Item1, East(Corner.b).Item2 - 1, East(Corner.b).Item3 - 1)
        East(Corner.c) = (East(Corner.c).Item1, East(Corner.c).Item2 + 1, East(Corner.c).Item3 - 1)
        East(Corner.d) = (East(Corner.d).Item1, East(Corner.d).Item2 + 1, East(Corner.d).Item3 + 1)

        Bottom_a = bottom(Corner.a)
        Bottom_b = bottom(Corner.b)
        Bottom_c = bottom(Corner.c)
        Bottom_d = bottom(Corner.d)
        Top_a = Top(Corner.a)
        Top_b = Top(Corner.b)
        Top_c = Top(Corner.c)
        Top_d = Top(Corner.d)
        North_a = North(Corner.a)
        North_b = North(Corner.b)
        North_c = North(Corner.c)
        North_d = North(Corner.d)
        South_a = South(Corner.a)
        South_b = South(Corner.b)
        South_c = South(Corner.c)
        South_d = South(Corner.d)
        West_a = West(Corner.a)
        West_b = West(Corner.b)
        West_c = West(Corner.c)
        West_d = West(Corner.d)
        East_a = East(Corner.a)
        East_b = East(Corner.b)
        East_c = East(Corner.c)
        East_d = East(Corner.d)

        panelsArray = New(PanelType, (Integer, Integer, Integer), (Integer, Integer, Integer))() {
                (PanelType.BottomPanel, Bottom_a, Bottom_c),
                (PanelType.NorthPanel, North_d, North_b),
                (PanelType.EastPanel, East_d, East_b),
                (PanelType.SouthPanel, South_d, South_b),
                (PanelType.WestPanel, West_d, West_b),
                (PanelType.TopPanel, Top_d, Top_b)
            }

        panelNormalsArray = New(PanelType, (Integer, Integer, Integer), (Integer, Integer, Integer))() {
                (PanelType.BottomPanel, CalculateNormal(Bottom_a, Bottom_b, Bottom_c), Bottom_c),
                (PanelType.NorthPanel, CalculateNormal(North_a, North_b, North_c), North_c),
                (PanelType.EastPanel, CalculateNormal(East_a, East_b, East_c), East_c),
                (PanelType.SouthPanel, CalculateNormal(South_a, South_b, South_c), South_c),
                (PanelType.WestPanel, CalculateNormal(West_a, West_b, West_c), West_c),
                (PanelType.TopPanel, CalculateNormal(Top_a, Top_b, Top_c), Top_c)
            }

        panelName_CornerArray = New(PanelCorner, (Integer, Integer, Integer))() {
                (PanelCorner.Bottom_a, Bottom_a),
                (PanelCorner.Bottom_b, Bottom_b),
                (PanelCorner.Bottom_c, Bottom_c),
                (PanelCorner.Bottom_d, Bottom_d),
                (PanelCorner.North_a, North_a),
                (PanelCorner.North_b, North_b),
                (PanelCorner.North_c, North_c),
                (PanelCorner.North_d, North_d),
                (PanelCorner.East_a, East_a),
                (PanelCorner.East_b, East_b),
                (PanelCorner.East_c, East_c),
                (PanelCorner.East_d, East_d),
                (PanelCorner.South_a, South_a),
                (PanelCorner.South_b, South_b),
                (PanelCorner.South_c, South_c),
                (PanelCorner.South_d, South_d),
                (PanelCorner.West_a, West_a),
                (PanelCorner.West_b, West_b),
                (PanelCorner.West_c, West_c),
                (PanelCorner.West_d, West_d),
                (PanelCorner.Top_a, Top_a),
                (PanelCorner.Top_b, Top_b),
                (PanelCorner.Top_c, Top_c),
                (PanelCorner.Top_d, Top_d)
            }
    End Sub

    Function CalculateNormal(firstCorner As (Integer, Integer, Integer), secondCorner As (Integer, Integer, Integer), thirdCorner As (Integer, Integer, Integer)) As (Integer, Integer, Integer)
        Dim edgeVectorFirstToSecond = (secondCorner.Item1 - firstCorner.Item1, secondCorner.Item2 - firstCorner.Item2, secondCorner.Item3 - firstCorner.Item3)
        Dim edgeVectorFirstToThird = (thirdCorner.Item1 - firstCorner.Item1, thirdCorner.Item2 - firstCorner.Item2, thirdCorner.Item3 - firstCorner.Item3)
        Dim crossProduct = (edgeVectorFirstToSecond.Item2 * edgeVectorFirstToThird.Item3 - edgeVectorFirstToSecond.Item3 * edgeVectorFirstToThird.Item2,
                             edgeVectorFirstToSecond.Item3 * edgeVectorFirstToThird.Item1 - edgeVectorFirstToSecond.Item1 * edgeVectorFirstToThird.Item3,
                             edgeVectorFirstToSecond.Item1 * edgeVectorFirstToThird.Item2 - edgeVectorFirstToSecond.Item2 * edgeVectorFirstToThird.Item1)
        Return crossProduct ' Computes the normal vector for a panel from three corners
    End Function

    Public Function TopPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = Top_a ' Returns the (x, y, z) coordinate on the Panel for a given row and column.
        Dim rowEnd = Top_b
        Dim colEnd = Top_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Public Function BottomPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = Bottom_a
        Dim rowEnd = Bottom_b
        Dim colEnd = Bottom_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Public Function NorthPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = North_a
        Dim rowEnd = North_b
        Dim colEnd = North_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Public Function SouthPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = South_a
        Dim rowEnd = South_b
        Dim colEnd = South_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Public Function EastPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = East_a
        Dim rowEnd = East_b
        Dim colEnd = East_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Public Function WestPanelRowColToXYZ(row As Integer, col As Integer) As (Integer, Integer, Integer)
        Dim origin = West_a
        Dim rowEnd = West_b
        Dim colEnd = West_d
        Return PanelRowColToXYZ(origin, rowEnd, colEnd, col, row)
    End Function
    Private Function PanelRowColToXYZ(origin As (Integer, Integer, Integer), rowEnd As (Integer, Integer, Integer), colEnd As (Integer, Integer, Integer), row As Integer, col As Integer) As (Integer, Integer, Integer)

        ' Calculate the step direction for each axis (always integer: -1, 0, or 1)
        Dim rowStep = (Sign(rowEnd.Item1 - origin.Item1), Sign(rowEnd.Item2 - origin.Item2), Sign(rowEnd.Item3 - origin.Item3))
        Dim colStep = (Sign(colEnd.Item1 - origin.Item1), Sign(colEnd.Item2 - origin.Item2), Sign(colEnd.Item3 - origin.Item3))

        Dim x = origin.Item1 + (row - 1) * rowStep.Item1 + (col - 1) * colStep.Item1 ' (row - 1) & (col -1) for 1-based indexing
        Dim y = origin.Item2 + (row - 1) * rowStep.Item2 + (col - 1) * colStep.Item2
        Dim z = origin.Item3 + (row - 1) * rowStep.Item3 + (col - 1) * colStep.Item3

        Return (x, y, z)
    End Function
    Private Function Sign(value As Integer) As Integer
        If value > 0 Then Return 1
        If value < 0 Then Return -1
        Return 0
    End Function
    Private Function GetExtremeIndex(panel As PanelType, itemIndex As Integer, useMin As Boolean) As Integer
        Dim grid = GetPanelGrid(panel)
        If grid.Count = 0 Then Throw New InvalidOperationException("No keys found for the specified panel.")

        Dim extreme As Integer = If(useMin, Integer.MaxValue, Integer.MinValue)
        For Each key In grid.Keys
            Dim value As Integer
            If itemIndex = 1 Then
                value = key.Item1 ' Row
            ElseIf itemIndex = 2 Then
                value = key.Item2 ' Col
            Else
                Throw New ArgumentException("itemIndex must be 1 (row) or 2 (col)")
            End If

            If useMin Then
                If value < extreme Then extreme = value
            Else
                If value > extreme Then extreme = value
            End If
        Next
        Return extreme
    End Function
    Private Function GetPanelGrid(panel As PanelType) As Dictionary(Of (Integer, Integer), (Integer, Integer, Integer))
        Select Case panel
            Case PanelType.TopPanel
                Return TopPanelGrid
            Case PanelType.BottomPanel
                Return BottomPanelGrid
            Case PanelType.NorthPanel
                Return NorthPanelGrid
            Case PanelType.SouthPanel
                Return SouthPanelGrid
            Case PanelType.EastPanel
                Return EastPanelGrid
            Case PanelType.WestPanel
                Return WestPanelGrid
            Case Else
                Throw New ArgumentException("Invalid panel type")
        End Select
    End Function

    Public Function GetFurthestLeftColumnIndex(panel As PanelType) As Integer
        Return GetExtremeIndex(panel, itemIndex:=2, useMin:=True)
    End Function
    Public Function GetFurthestRightColumnIndex(panel As PanelType) As Integer
        Return GetExtremeIndex(panel, itemIndex:=2, useMin:=False)
    End Function
    Public Function GetTopmostRowIndex(panel As PanelType) As Integer
        Return GetExtremeIndex(panel, itemIndex:=1, useMin:=True)
    End Function
    Public Function GetBottommostRowIndex(panel As PanelType) As Integer
        Return GetExtremeIndex(panel, itemIndex:=1, useMin:=False)
    End Function

End Class

#End Region

Public Module ThinningDebugHelpers

    ' Helper method: writes to Console what each parameter "needs/expects"
    ' and what was actually provided.
    Public Sub DumpThinEvenSpatiallyAdaptiveAutoSignatureRequirements(
        ByRef sourceDict As ConcurrentDictionary(Of Integer, MyObject),
        ByRef destDict As ConcurrentDictionary(Of Integer, MyObject),
        numToLeave As Integer,
        observer As (Integer, Integer, Integer),
        keepRadius As Double,
        Optional numBands As Integer = 10,
        Optional closeBiasExponent As Double = 1.5
    )

        Console.WriteLine("===== ThinEvenSpatiallyAdaptiveAuto :: Signature diagnostics =====")
        Console.WriteLine($"Timestamp (UTC): {DateTime.UtcNow:O}")
        Console.WriteLine()

        ' ---- sourceDict ----
        Console.WriteLine("PARAM: sourceDict As ConcurrentDictionary(Of Integer, MyObject) (ByRef)")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - Not Nothing")
        Console.WriteLine("    - Keys are Integer")
        Console.WriteLine("    - Values are not Nothing")
        Console.WriteLine("    - Used for: Keys.ToArray(), Count, indexing: sourceDict(key), TryRemove")
        If sourceDict Is Nothing Then
            Console.WriteLine("  Actual: Nothing")
        Else
            Console.WriteLine($"  Actual: Count={sourceDict.Count}")
            Dim sampleKeys = sourceDict.Keys.Take(5).ToArray()
            Console.WriteLine($"          Sample Keys (up to 5): {(If(sampleKeys.Length = 0, "<none>", String.Join(", ", sampleKeys)))}")

            Dim nullValueCount As Integer = 0
            Dim missingLocationCount As Integer = 0
            Dim sampleLocs As New List(Of String)()

            For Each kvp In sourceDict.Take(25)
                If kvp.Value Is Nothing Then
                    nullValueCount += 1
                Else
                    ' We cannot know the exact type of Location at compile time here beyond what's used:
                    ' obj.Location.X/Y/Z must be readable and convertible to Long via CLng(...)
                    Try
                        Dim x As Long = CLng(kvp.Value.Location.X)
                        Dim y As Long = CLng(kvp.Value.Location.Y)
                        Dim z As Long = CLng(kvp.Value.Location.Z)
                        If sampleLocs.Count < 5 Then sampleLocs.Add($"key={kvp.Key} -> ({x},{y},{z})")
                    Catch
                        missingLocationCount += 1
                    End Try
                End If
            Next

            Console.WriteLine($"          Null values among first 25 entries: {nullValueCount}")
            Console.WriteLine($"          Location/XYZ conversion failures among first 25 entries: {missingLocationCount}")
            If sampleLocs.Count > 0 Then
                Console.WriteLine("          Sample Locations (up to 5):")
                For Each s In sampleLocs
                    Console.WriteLine($"            - {s}")
                Next
            End If
        End If
        Console.WriteLine()

        ' ---- destDict ----
        Console.WriteLine("PARAM: destDict As ConcurrentDictionary(Of Integer, MyObject) (ByRef)")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - Not Nothing (even though this method currently does not add to it)")
        Console.WriteLine("    - Intended as storage for removed objects (comment says so)")
        If destDict Is Nothing Then
            Console.WriteLine("  Actual: Nothing")
        Else
            Console.WriteLine($"  Actual: Count={destDict.Count}")
        End If
        Console.WriteLine()

        ' ---- numToLeave ----
        Console.WriteLine("PARAM: numToLeave As Integer")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - > 0 (method exits early if <= 0)")
        Console.WriteLine("    - < sourceDict.Count to actually thin (method exits early if Count <= numToLeave)")
        Console.WriteLine($"  Actual: {numToLeave}")
        If numToLeave <= 0 Then
            Console.WriteLine("  Note: Method would Exit Sub because numToLeave <= 0.")
        End If
        If sourceDict IsNot Nothing AndAlso sourceDict.Count <= numToLeave Then
            Console.WriteLine("  Note: Method would Exit Sub because sourceDict.Count <= numToLeave.")
        End If
        Console.WriteLine()

        ' ---- observer ----
        Console.WriteLine("PARAM: observer As (Integer, Integer, Integer)")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - 3-tuple of Integers")
        Console.WriteLine("    - Used as a reference point, passed into Distance3D after CLng conversion")
        Console.WriteLine($"  Actual: ({observer.Item1}, {observer.Item2}, {observer.Item3})")
        Console.WriteLine()

        ' ---- keepRadius ----
        Console.WriteLine("PARAM: keepRadius As Double")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - Finite number (not NaN/Infinity)")
        Console.WriteLine("    - Typically >= 0 (negative values make 'd <= keepRadius' unlikely unless d is also negative)")
        Console.WriteLine("    - Used as threshold: if d <= keepRadius => band 0 and always kept")
        Console.WriteLine($"  Actual: {keepRadius}")
        If Double.IsNaN(keepRadius) OrElse Double.IsInfinity(keepRadius) Then
            Console.WriteLine("  Warning: keepRadius is not finite (NaN/Infinity).")
        ElseIf keepRadius < 0 Then
            Console.WriteLine("  Warning: keepRadius < 0; threshold logic may behave unexpectedly.")
        End If
        Console.WriteLine()

        ' ---- numBands ----
        Console.WriteLine("PARAM: numBands As Integer (Optional)")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - >= 2 (because code uses (numBands - 1) as a divisor and indexes bands 0..numBands-1)")
        Console.WriteLine("    - If = 1, (maxDist - keepRadius) / (numBands - 1) divides by zero")
        Console.WriteLine($"  Actual: {numBands}")
        If numBands < 2 Then
            Console.WriteLine("  Warning: numBands < 2 will break band calculation (division by zero / invalid indexing).")
        End If
        Console.WriteLine()

        ' ---- closeBiasExponent ----
        Console.WriteLine("PARAM: closeBiasExponent As Double (Optional)")
        Console.WriteLine("  Expects:")
        Console.WriteLine("    - Finite number (not NaN/Infinity)")
        Console.WriteLine("    - Typical: > 0")
        Console.WriteLine("    - Meaning: exponent > 1 biases keeping closer bands more strongly")
        Console.WriteLine($"  Actual: {closeBiasExponent}")
        If Double.IsNaN(closeBiasExponent) OrElse Double.IsInfinity(closeBiasExponent) Then
            Console.WriteLine("  Warning: closeBiasExponent is not finite (NaN/Infinity).")
        ElseIf closeBiasExponent <= 0 Then
            Console.WriteLine("  Warning: closeBiasExponent <= 0 may invert/flatten weighting behavior.")
        End If
        Console.WriteLine()

        ' ---- Derived expectations (based on how the method uses the signature) ----
        Console.WriteLine("DERIVED EXPECTATIONS (based on method body usage):")
        Console.WriteLine("  - sourceDict.Count and sourceDict.Keys.ToArray() must be safe to call.")
        Console.WriteLine("  - For each key used: sourceDict(key) must succeed during the distance/banding loops.")
        Console.WriteLine("  - Each MyObject should have Location with X/Y/Z readable and convertible via CLng.")
        Console.WriteLine("  - Distance3D must accept the tuple inputs and return a non-negative Double distance.")
        Console.WriteLine("  - When thinning: sourceDict.TryRemove(key, removed) is used; removed may be Nothing if key vanished concurrently.")
        Console.WriteLine("===============================================================")
    End Sub

End Module






