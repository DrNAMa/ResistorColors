Imports System
Imports System.Drawing.Drawing2D
Imports System.IO
Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices.JavaScript.JSType
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar
Imports Draw
Public Class Form1
    Private reval As Int64 'resistor value
    Private d As New Shapes
    Private b As Brush
    Private b1 As Brush
    Private b2 As Brush
    Private b3 As Brush
    Private b4 As Brush
    Private btol As Brush
    Private btemp As Brush
    Private rect1 As New Rectangle(400, 150, 75, 250)
    Private rect2 As New Rectangle(500, 150, 75, 250)
    Private rect3 As New Rectangle(600, 150, 75, 250)
    Private rect4 As New Rectangle(700, 150, 75, 250)
    Private rect5 As New Rectangle(1000, 150, 75, 250)
    Private rect6 As New Rectangle(1100, 150, 75, 250)
    Private g As Graphics
    Private numofbands As Integer
    Private rightbands As Integer
    Private sigdigs As Integer
    Private tolselected As RadioButton 'doubles as fail rate
    Private tempselected As RadioButton
    Private band1 As Brush
    Private band2 As Brush
    Private band3 As Brush
    Private band4 As Brush
    Private band5 As Brush
    Private band6 As Brush
    Private colorselected As Boolean
    Private bandselected As Integer
    Private bandselect As RadioButton
    Private bandsnumr As Integer
    Private bandsnuml As Integer
    Private loaded As Boolean
    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        loaded = True
        bandselect = rbcolsel1
    End Sub
    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown1.ValueChanged
        band1 = Nothing
        band2 = Nothing
        band3 = Nothing
        band4 = Nothing
        band5 = Nothing
        band6 = Nothing
        colorselected = False
        reval = NumericUpDown1.Value
        Splitnums(reval)
    End Sub
    Private Sub Splitnums(ByVal reval)
        If Not Me.Visible Then
            Exit Sub
        End If
        Dim len As Integer = reval.ToString.Length
        Dim splitnum As String
        Dim zeros As Integer = 0
        If reval > 9 Then
            splitnum = reval.ToString.Substring(sigdigs, len - sigdigs)
            zeros = CInt(splitnum.ToString.Length)
            'If splitnum Is Nothing Then
            '    Exit Sub
            'End If
        End If
        Select Case rightbands
            Case 2 : numofbands = 2
                b1 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 1, 1)))
                b2 = New SolidBrush(Selcol(zeros))
            Case 3 : numofbands = 3
                b1 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 2, 1)))
                b2 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 1, 1)))
                b3 = New SolidBrush(Selcol(zeros))
            Case 4 : numofbands = 4
                b1 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 3, 1)))
                b2 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 2, 1)))
                b3 = New SolidBrush(Selcol(reval.ToString().Substring(sigdigs - 1, 1)))
                b4 = New SolidBrush(Selcol(zeros))
        End Select
        Invalidate()
    End Sub
    Private Sub RBb1_CheckedChanged(sender As Object, e As EventArgs) Handles RBrb1.CheckedChanged
        If RBrb1.Checked Then
            rightbands = 2
            sigdigs = 1
            NumericUpDown1.Minimum = 1
            NumericUpDown1.Maximum = 1999999999
            If NumericUpDown1.Value > 0 And NumericUpDown1.Value < 2000000000 Then
            Else
                NumericUpDown1.Value = 1
            End If
        End If
    End Sub
    Private Sub RBrb2_CheckedChanged(sender As Object, e As EventArgs) Handles RBrb2.CheckedChanged
        If RBrb2.Checked Then
            rightbands = 3
            sigdigs = 2
            NumericUpDown1.Minimum = 10
            NumericUpDown1.Maximum = 19999999999
            If NumericUpDown1.Value > 9 And NumericUpDown1.Value < 20000000000 Then
            Else
                NumericUpDown1.Value = 10
            End If
        End If
    End Sub
    Private Sub RBrb3_CheckedChanged(sender As Object, e As EventArgs) Handles RBrb3.CheckedChanged
        If RBrb3.Checked Then
            rightbands = 4
            sigdigs = 3
            NumericUpDown1.Minimum = 100
            NumericUpDown1.Maximum = 199999999999
            If NumericUpDown1.Value > 99 And NumericUpDown1.Value < 200000000000 Then
            Else
                NumericUpDown1.Value = 100
            End If
        End If
    End Sub
    Private Sub RBtol1_CheckedChanged(sender As Object, e As EventArgs) Handles rbtol1.CheckedChanged, rbtol2.CheckedChanged, rbtol3.CheckedChanged, rbtol4.CheckedChanged, rbtol5.CheckedChanged, rbtol6.CheckedChanged, rbtol7.CheckedChanged, rbtol8.CheckedChanged, rbtol9.CheckedChanged
        Dim rad As RadioButton = DirectCast(sender, RadioButton)
        If Not rad.Checked Then
            Exit Sub
        End If
        For Each c As Control In GroupBox4.Controls
            If TypeOf c Is RadioButton Then
                DirectCast(c, RadioButton).Checked = False
            End If
        Next
        tolselected = rad
        If tolselected IsNot Nothing Then btol = New SolidBrush(Selcol(tolselected.Tag))
        Invalidate()
    End Sub
    Private Sub RBfail1_CheckedChanged(sender As Object, e As EventArgs) Handles rbfail1.CheckedChanged, rbfail2.CheckedChanged, rbfail3.CheckedChanged, rbfail4.CheckedChanged, rbfail5.CheckedChanged
        Dim rad As RadioButton = DirectCast(sender, RadioButton)
        If Not rad.Checked Then
            Exit Sub
        End If
        For Each c As Control In GroupBox2.Controls
            If TypeOf c Is RadioButton Then
                DirectCast(c, RadioButton).Checked = False
            End If
        Next
        tolselected = rad
        If tolselected IsNot Nothing Then btol = New SolidBrush(Selcol(tolselected.Tag))
        Invalidate()
    End Sub
    Private Sub RBtemp1_CheckedChanged(sender As Object, e As EventArgs) Handles rbtemp1.CheckedChanged, rbtemp2.CheckedChanged, rbtemp3.CheckedChanged, rbtemp4.CheckedChanged, rbtemp5.CheckedChanged, rbtemp6.CheckedChanged, rbtemp7.CheckedChanged, rbtemp8.CheckedChanged, rbtemp9.CheckedChanged, rbtemp10.CheckedChanged
        Dim rad As RadioButton = DirectCast(sender, RadioButton)
        tempselected = rad
        If tempselected IsNot Nothing Then btemp = New SolidBrush(Selcol(tempselected.Tag))
        Invalidate()
    End Sub
    'Bottom Color Selector 
    Private Sub RBcolsel1_CheckedChanged(sender As Object, e As EventArgs) Handles rbcolsel1.CheckedChanged, rbcolsel2.CheckedChanged, rbcolsel3.CheckedChanged, rbcolsel4.CheckedChanged, rbcolsel5.CheckedChanged, rbcolsel6.CheckedChanged
        If Not loaded Then
            Exit Sub
        End If
        bandselect = DirectCast(sender, RadioButton)
        bandselected = CInt(bandselect.Name.Split("rbcolsel").Last())
    End Sub
    Private Sub Cs1_Click(sender As Object, e As EventArgs) Handles cs1.Click, cs2.Click, cs3.Click, cs4.Click, cs5.Click, cs6.Click, cs7.Click, cs8.Click, cs9.Click, cs10.Click, cs11.Click, cs12.Click
        Dim pan As Panel = DirectCast(sender, Panel)
        BandSelector(bandselect.Tag, pan.Tag)
        colorselected = True
        Dim a As (total As String, op As String) = CheckTotal(TranslateColor2Number())
        Dim num As Decimal
        Dim formattedd As String = String.Empty
        If Decimal.TryParse(a.total, num) Then
            formattedd = num.ToString("#,##0.############")
        End If
        '  Dim formatted As String = a.total
        TextBox1.Text = formattedd & "Ω " & a.op
        Invalidate()
    End Sub
    Private Sub Bnocol_Click(sender As Object, e As EventArgs) Handles bnocol.Click
        Dim but As Button = DirectCast(sender, Button)
        BandSelector(bandselect.Tag, but.Tag)
        Dim a As (total As String, op As String) = CheckTotal(TranslateColor2Number())
        Dim num As Decimal
        Dim formattedd As String = String.Empty
        If Decimal.TryParse(a.total, num) Then
            formattedd = num.ToString("#,##0.############")
        End If
        TextBox1.Text = formattedd & "Ω " & a.op
        colorselected = True
        Invalidate()
    End Sub
    Private Function CheckTotal(val() As String) As (String, String)
        Dim tempval() As String = val
        Dim newnum As String = String.Empty
        For i = 0 To 3
            If Not tempval(i) = String.Empty Then
                newnum &= tempval(i)
            End If
        Next
        Dim opers() As String = Selectoperator(val)
        Dim newval As String = CheckIntensity(newnum)
        Dim newstrang As String = opers(0) & " " & opers(1)
        Return (newval, newstrang)
    End Function
    Private Function CheckIntensity(vall As String) As String
        Dim valholder As String = vall
        If valholder.Contains("T") Then
            valholder = valholder.Replace("T", "")
        ElseIf valholder.Contains("G") Then
            valholder = valholder.Replace("G", "")
        ElseIf valholder.Contains("S") Then
            valholder = valholder.Replace("S", "")
        End If
        If valholder = String.Empty Then
            Exit Function
        End If
        Dim howmanyzeros As Integer
        Dim tempnumb As String = String.Empty
        If Val(valholder).ToString.Length = 1 Then
            howmanyzeros = 0
            If valholder.First().Equals("0"c) Then
                Dim tempval As String = valholder.First().ToString.Replace("0", "0.")
                valholder = tempval & valholder.Remove(0, 1)
            End If
            tempnumb = valholder
        Else
            howmanyzeros = valholder.Last().ToString
            If valholder.First().Equals("0"c) Then
                Dim tempval As String = valholder.First().ToString.Replace("0", "0.")
                valholder = tempval & valholder.Remove(0, 1)
            Else
                tempnumb = valholder.Remove(valholder.Length - 1, 1)
            End If
            ' valholder.Remove(valholder.Length - 1, 1)
        End If
        Dim newnumber As String = tempnumb & New String("0"c, howmanyzeros)
        Return newnumber
    End Function
    Private Function TranslateColor2Number() As String()
        Dim nums(3) As String
        Dim newval(1) As String
        If band1 IsNot Nothing Then
            nums(0) = SelectBrushColor(DirectCast(band1, SolidBrush).Color)
        Else nums(0) = String.Empty
        End If
        If band2 IsNot Nothing Then
            nums(1) = SelectBrushColor(DirectCast(band2, SolidBrush).Color)
        Else nums(1) = String.Empty
        End If
        If band3 IsNot Nothing Then
            nums(2) = SelectBrushColor(DirectCast(band3, SolidBrush).Color)
        Else nums(2) = String.Empty
        End If
        If band4 IsNot Nothing Then
            nums(3) = SelectBrushColor(DirectCast(band4, SolidBrush).Color)
        Else nums(3) = String.Empty
        End If
        If band5 IsNot Nothing Then
            newval(0) = SelectBrushColor(DirectCast(band5, SolidBrush).Color)
        Else newval(0) = String.Empty
        End If
        If band6 IsNot Nothing Then
            newval(1) = SelectBrushColor(DirectCast(band6, SolidBrush).Color)
        Else newval(1) = String.Empty
        End If
        Dim combinedval() As String = nums
        ReDim Preserve combinedval(5)
        Array.Copy(newval, 0, combinedval, 4, newval.Length)
        Return combinedval
    End Function
    Private Function SelectBrushColor(c As Color) As String
        Select Case c
            Case Color.Black : Return 0
            Case Color.Brown : Return 1
            Case Color.Red : Return 2
            Case Color.Orange : Return 3
            Case Color.Yellow : Return 4
            Case Color.Green : Return 5
            Case Color.Blue : Return 6
            Case Color.Violet : Return 7
            Case Color.Gray : Return 8
            Case Color.White : Return 9
            Case Color.Gold : Return "G"
            Case Color.Silver : Return "S"
            Case Color.Transparent : Return "T"
            Case Else
                Return "T"
        End Select
    End Function
    Private Sub BandSelector(tt As String, t As String)
        Select Case tt
            Case "1"
                band1 = New SolidBrush(Selcol(t))
            Case "2"
                band2 = New SolidBrush(Selcol(t))
            Case "3"
                band3 = New SolidBrush(Selcol(t))
            Case "4"
                band4 = New SolidBrush(Selcol(t))
            Case "5"
                band5 = New SolidBrush(Selcol(t))
            Case "6"
                band6 = New SolidBrush(Selcol(t))
        End Select
    End Sub
    Private Function Selectoperator(val() As String) As String()
        Dim band5color As SolidBrush
        band5color = CType(band5, SolidBrush)
        Dim band6color As SolidBrush
        band6color = CType(band6, SolidBrush)
        Dim newval(1) As String
        Dim valslast As String = val.Last().ToString
        Select Case val(4)
            Case 1
                newval(0) = " ±1%T or 1%FR"
            Case 2
                newval(0) = " ±2%T or 0.1%FR"
            Case 3
                newval(0) = " 0.01%FR"
            Case 4
                newval(0) = " 0.001%FR"
            Case 5
                newval(0) = " ±0.5%T"
            Case 6
                newval(0) = " ±0.25%T"
            Case 7
                newval(0) = " ±0.1%T"
            Case 8
                newval(0) = " ±0.05%T"
            Case "G"
                newval(0) = " ±5%T"
            Case "S"
                newval(0) = " ±10%T"
            Case "T"
                newval(0) = " ±20%T"
            Case Else
                newval(0) = " ±20%T"
        End Select
        Select Case val(5)
            Case 0
                newval(1) = " 250TC"
            Case 1
                newval(1) = " 100TC"
            Case 2
                newval(1) = " 50TC"
            Case 3
                newval(1) = " 15TC"
            Case 4
                newval(1) = " 25TC"
            Case 5
                newval(1) = " 20TC"
            Case 6
                newval(1) = " 10TC"
            Case 7
                newval(1) = " 5TC"
            Case 8
                newval(1) = " 1TC"
        End Select
        Return newval
    End Function
    Private Function Selcol(ByVal digit As String) As Color
        Select Case digit
            Case "0"
                Return Color.Black
            Case "1"
                Return Color.Brown
            Case "2"
                Return Color.Red
            Case "3"
                Return Color.Orange
            Case "4"
                Return Color.Yellow
            Case "5"
                Return Color.Green
            Case "6"
                Return Color.Blue
            Case "7"
                Return Color.Violet
            Case "8"
                Return Color.Gray
            Case "9"
                Return Color.White
            Case "10"
                Return Color.Gold
            Case "11"
                Return Color.Silver
            Case "T", "00"
                Return Color.Transparent
        End Select
    End Function
    'End Bottom Color Selector

    Public Sub Form1_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        Dim rect As New Rectangle(300, 150, 1000, 250)
        Dim b As Brush = New SolidBrush(Color.NavajoWhite)
        d.DrawFilledRectangle(b, rect, e)
        ' === 1 Shape(s), 10 Pts | Shift(0.0, -75.0) | Bounds(X:-43.2, Y:230.6, W:343.2, H:84.8) ===
        Dim transformedPoints As PointF() = {
    New PointF(165.5F, 255.0F),
    New PointF(235.7F, 239.4F),
    New PointF(300.0F, 230.6F),
    New PointF(300.0F, 270.1F),
    New PointF(300.0F, 315.4F),
    New PointF(247.5F, 310.0F),
    New PointF(201.3F, 303.4F),
    New PointF(164.7F, 296.1F),
    New PointF(-43.2F, 293.6F),
    New PointF(-42.7F, 254.4F)
}
        Using shapeBrush As New SolidBrush(Color.Silver)
            e.Graphics.FillPolygon(shapeBrush, transformedPoints)
        End Using
        ' === 1 Shape(s), 10 Pts | Shift(+44.0, 0.0) | Bounds(X:1300.8, Y:230.6, W:343.2, H:84.8) ===
        Dim transformedPoints2 As PointF() = {
    New PointF(1435.3F, 255.0F),
    New PointF(1365.1F, 239.4F),
    New PointF(1299.8F, 230.6F),
    New PointF(1299.8F, 270.1F),
    New PointF(1299.8F, 315.4F),
    New PointF(1353.3F, 310.0F),
    New PointF(1399.5F, 303.4F),
    New PointF(1436.1F, 296.1F),
    New PointF(1644.0F, 293.6F),
    New PointF(1643.5F, 254.4F)
}
        Using shapeBrush As New SolidBrush(Color.Silver)
            e.Graphics.FillClosedCurve(shapeBrush, transformedPoints2, FillMode.Alternate, 0.5F)
        End Using
        GC.Collect()
    End Sub
    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        MyBase.OnPaint(e)
        g = e.Graphics
        g.SmoothingMode = Drawing.Drawing2D.SmoothingMode.AntiAlias
        If colorselected = False Then
            Select Case numofbands
            'Case 1
            '    d.DrawFilledRectangle(b1, rect1, e) 'resistor
                Case 2
                    d.DrawFilledRectangle(b1, rect1, e)
                    d.DrawFilledRectangle(b2, rect2, e)
                   ' d.DrawFilledRectangle(btol, rect5, e)
                   ' d.DrawFilledRectangle(btemp, rect6, e)
                Case 3
                    d.DrawFilledRectangle(b1, rect1, e)
                    d.DrawFilledRectangle(b2, rect2, e)
                    d.DrawFilledRectangle(b3, rect3, e)
                Case 4
                    d.DrawFilledRectangle(b1, rect1, e)
                    d.DrawFilledRectangle(b2, rect2, e)
                    d.DrawFilledRectangle(b3, rect3, e)
                    d.DrawFilledRectangle(b4, rect4, e)
                    '  d.DrawFilledRectangle(btol, rect5, e)
                    ' d.DrawFilledRectangle(btemp, rect6, e)
            End Select
            d.DrawFilledRectangle(If(btol, Brushes.Transparent), rect5, e)
            d.DrawFilledRectangle(If(btemp, Brushes.Transparent), rect6, e)
            'trying out setting these brushes to nothing
            btol = Nothing
            btemp = Nothing
        Else
            If band1 IsNot Nothing Then
                Dim band1color As SolidBrush
                band1color = CType(band1, SolidBrush)
                If band1color.Color.ToArgb() = Color.Gold.ToArgb() Or band1color.Color.ToArgb() = Color.Silver.ToArgb() Or band1color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band1, rect1, e)
                    bandsnumr += 1
                End If
            End If
            If band2 IsNot Nothing Then
                Dim band2color As SolidBrush
                band2color = CType(band2, SolidBrush)
                If band2color.Color.ToArgb() = Color.Gold.ToArgb() Or band2color.Color.ToArgb() = Color.Silver.ToArgb() Or band2color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band2, rect2, e)
                    bandsnumr += 1
                End If
            End If
            If band3 IsNot Nothing Then
                Dim band3color As SolidBrush
                band3color = CType(band3, SolidBrush)
                If band3color.Color.ToArgb() = Color.Gold.ToArgb() Or band3color.Color.ToArgb() = Color.Silver.ToArgb() Or band3color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band3, rect3, e)
                    bandsnumr += 1
                End If
            End If
            If band4 IsNot Nothing Then
                Dim band4color As SolidBrush
                band4color = CType(band4, SolidBrush)
                If band4color.Color.ToArgb() = Color.Gold.ToArgb() Or band4color.Color.ToArgb() = Color.Silver.ToArgb() Or band4color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band4, rect4, e)
                    bandsnumr += 1
                End If
            End If
            If band5 IsNot Nothing Then
                Dim band5color As SolidBrush
                band5color = CType(band5, SolidBrush)
                If band5color.Color.ToArgb() = Color.White.ToArgb() Or band5color.Color.ToArgb() = Color.Black.ToArgb() Or band5color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band5, rect5, e)
                    bandsnuml += 1
                End If
            End If
            If band6 IsNot Nothing Then
                Dim band6color As SolidBrush
                band6color = CType(band6, SolidBrush)
                If band6color.Color.ToArgb() = Color.White.ToArgb() Or band6color.Color.ToArgb() = Color.Gold.ToArgb() Or band6color.Color.ToArgb() = Color.Silver.ToArgb() Or band6color.Color.ToArgb() = Color.Transparent.ToArgb() Then
                Else
                    d.DrawFilledRectangle(band6, rect6, e)
                    bandsnuml += 1
                End If
            End If
        End If
    End Sub
End Class
Public Class Shapedraw
    Public Property Points As List(Of PointF)
    Public Property Tension As Single = 0.5F

    Private Function BuildPath() As GraphicsPath
        Dim path As New GraphicsPath()
        ' If IsClosed AndAlso Points.Count >= 3 Then
        path.AddClosedCurve(Points.ToArray(), Tension)
        '  Else
        ''path.AddCurve(Points.ToArray(), Tension)
        '  End If
        Return path
    End Function


    'Using path As GraphicsPath = BuildPath()
    'If Points.Count >= 3 Then
    'Using fillBrush As New SolidBrush(Color.FromArgb(ShapeColor))
    '                g.FillPath(fillBrush, path)
    '            End Using
    'End If


    'Using glowPen As New Pen(Color.FromArgb(65, ShapeColor), 10.0F)
    '            glowPen.LineJoin = LineJoin.Round
    '            g.DrawPath(glowPen, path)
    '        End Using


    'Using mainPen As New Pen(ShapeColor)
    '            g.DrawPath(mainPen, path)
    '        End Using
    'End Using
End Class