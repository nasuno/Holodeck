

Imports System.Numerics
Imports System.Threading
Imports Current.PluginApi

Module TestStructures








    Public Sub Create3DTestObject()
        ' Three hardcoded line coordinates (user-specified)
        Dim lines As New List(Of Tuple(Of (Integer, Integer, Integer), (Integer, Integer, Integer))) From {
        New Tuple(Of (Integer, Integer, Integer), (Integer, Integer, Integer))((-374, 223, -146), (-374, 223, -139)),
        New Tuple(Of (Integer, Integer, Integer), (Integer, Integer, Integer))((-374, 220, -146), (-374, 220, -139)),
        New Tuple(Of (Integer, Integer, Integer), (Integer, Integer, Integer))((-374, 217, -146), (-374, 217, -139))
    }

        For i As Integer = 0 To lines.Count - 1
            Dim setId As Integer = GetNextUniqId()
            Dim a = lines(i).Item1
            Dim b = lines(i).Item2
            Dim points = Bresenham3D(a.Item1, a.Item2, a.Item3, b.Item1, b.Item2, b.Item3)
            For Each pt In points
                AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, setId)
            Next

            ' ColorOverride: first line = None, second = Black, third = Yellow
            If i = 1 OrElse i = 2 Then
                Dim colorValue As ObjectColor = If(i = 1, ObjectColor.Black, ObjectColor.Yellow)
                Dim objectIds As Immutable.ImmutableList(Of Integer) = Nothing
                If structureObjectIDs.TryGetValue(setId, objectIds) Then
                    For Each objId In objectIds
                        Dim obj As MyObject = Nothing
                        If objectDictionary.TryGetValue(objId, obj) Then
                            obj.ColorOverride = colorValue
                        End If
                    Next
                End If
            End If
        Next
    End Sub






















    Sub CreateGridLinesGrid(centerX As Double, centerY As Double, centerZ As Double, structureId As Integer,
                       Optional lineCountX As Integer = 10, Optional lineCountZ As Integer = 10,
                       Optional spacing As Double = 2000, Optional lineLength As Integer = 1000000)

        ' The grid plane y coordinate is centerY - 2000
        Dim gridY As Double = centerY - 8000

        ' Calculate starting positions for the lines to ensure the grid is centered
        Dim halfGridWidthX As Double = ((lineCountX - 1) * spacing) / 2
        Dim halfGridWidthZ As Double = ((lineCountZ - 1) * spacing) / 2

        ' X-axis lines: Vary X, keep Z constant for each line
        For i = 0 To lineCountZ - 1
            Dim lineZ As Double = centerZ - halfGridWidthZ + i * spacing
            For p = 0 To lineLength - 1
                Dim x As Double = centerX - (lineLength / 2) * 1 + p * 1 ' 1 unit between points
                AddMyObjectToFactory(x, gridY, lineZ, structureId)
            Next
        Next

        ' Z-axis lines: Vary Z, keep X constant for each line
        For i = 0 To lineCountX - 1
            Dim lineX As Double = centerX - halfGridWidthX + i * spacing
            For p = 0 To lineLength - 1
                Dim z As Double = centerZ - (lineLength / 2) * 1 + p * 1 ' 1 unit between points
                AddMyObjectToFactory(lineX, gridY, z, structureId)
            Next
        Next

        structureDrawState.TryAdd(structureId, True)
    End Sub





    Sub CreateUCSIcon()
        Dim structureId As Integer = 1 ' Unique ID for UCS Icon structure
        For i = 0 To 1000000 Step 1
            AddMyObjectToFactory(i, 0, 0, structureId) ' Points on X-axis
            AddMyObjectToFactory(0, i, 0, structureId) ' Points on Y-axis
            AddMyObjectToFactory(0, 0, i, structureId) ' Points on Z-axis
        Next i
        ' Optionally initialize draw state
        structureDrawState.TryAdd(structureId, True)
    End Sub

    'Sub CreateBarExtensions()
    '    AddMyObjectToFactory(-129, 160, -69)
    '    AddMyObjectToFactory(-128, 160, -69)
    '    AddMyObjectToFactory(-127, 160, -69)
    '    AddMyObjectToFactory(-126, 160, -69)
    '    AddMyObjectToFactory(-125, 160, -69)
    '    Dim i As Integer
    '    For i = -124 To -50
    '        AddMyObjectToFactory(i, 160, -69)
    '    Next
    '    For i = 161 To 165
    '        AddMyObjectToFactory(-50, i, -69)
    '    Next
    '    For i = 159 To 155 Step -1
    '        AddMyObjectToFactory(-50, i, -69)
    '    Next
    '    AddMyObjectToFactory(-371, 160, -69)
    '    AddMyObjectToFactory(-372, 160, -69)
    '    AddMyObjectToFactory(-373, 160, -69)
    '    AddMyObjectToFactory(-374, 160, -69)
    '    AddMyObjectToFactory(-375, 160, -69)
    '    For i = -376 To -450 Step -1
    '        AddMyObjectToFactory(i, 160, -69)
    '    Next
    'End Sub




    ' Fills the objectDictionary with objects distributed randomly in a spherical shell
    Sub scattering()
        Dim centerX As Integer = -250
        Dim centerY As Integer = 155
        Dim centerZ As Integer = -78
        Dim innerRadius As Double = 1000.0
        Dim outerRadius As Double = 50000.0
        Dim numPoints As Integer = 10000
        Dim structureId As Integer = 2 ' Unique ID for scattering group
        Dim generator As New SphericalShellPointsGenerator(centerX, centerY, centerZ, innerRadius, outerRadius, numPoints, structureId)

        structureDrawState.TryAdd(structureId, True)
    End Sub

    ' Update SphericalShellPointsGenerator to accept structureId, and use it in AddMyObjectToFactory
    Public Class SphericalShellPointsGenerator
        Private centerX As Integer
        Private centerY As Integer
        Private centerZ As Integer
        Private innerRadius As Double
        Private outerRadius As Double
        Private numPoints As Integer
        Private structureId As Integer ' <-- New

        ' Refactored constructor to accept structureId
        Public Sub New(centerX As Integer, centerY As Integer, centerZ As Integer, innerRadius As Double, outerRadius As Double, numPoints As Integer, structureId As Integer)
            Me.centerX = centerX
            Me.centerY = centerY
            Me.centerZ = centerZ
            Me.innerRadius = innerRadius
            Me.outerRadius = outerRadius
            Me.numPoints = numPoints
            Me.structureId = structureId
            GeneratePoints()
        End Sub

        ' Generates random integer points within a spherical shell and adds them to the object factory
        Private Sub GeneratePoints()
            Dim rand As New Random()
            Dim generatedPoints As New HashSet(Of String)()
            While generatedPoints.Count < numPoints
                Dim r As Double = innerRadius + (outerRadius - innerRadius) * rand.NextDouble()
                Dim theta As Double = Math.Acos(1 - 2 * rand.NextDouble())
                Dim phi As Double = 2 * Math.PI * rand.NextDouble()
                Dim xDouble As Double = r * Math.Sin(theta) * Math.Cos(phi)
                Dim yDouble As Double = r * Math.Sin(theta) * Math.Sin(phi)
                Dim zDouble As Double = r * Math.Cos(theta)
                Dim x As Integer = CInt(Math.Round(xDouble + centerX))
                Dim y As Integer = CInt(Math.Round(yDouble + centerY))
                Dim z As Integer = CInt(Math.Round(zDouble + centerZ))
                Dim pointKey As String = $"{x},{y},{z}"
                Dim dx As Double = x - centerX
                Dim dy As Double = y - centerY
                Dim dz As Double = z - centerZ
                Dim distanceSquared As Double = dx * dx + dy * dy + dz * dz
                Dim innerRadiusSquared As Double = innerRadius * innerRadius
                Dim outerRadiusSquared As Double = outerRadius * outerRadius
                If distanceSquared >= innerRadiusSquared AndAlso distanceSquared <= outerRadiusSquared Then
                    If Not generatedPoints.Contains(pointKey) Then
                        generatedPoints.Add(pointKey)
                        AddMyObjectToFactory(x, y, z, structureId) ' <-- Pass structureId
                    End If
                End If
            End While
        End Sub
    End Class













End Module

