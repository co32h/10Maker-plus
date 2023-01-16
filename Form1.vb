Public Class TenMaker

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Dim StrOrigin As String
    Dim ten As Integer = 10
    Dim IntOrigin As Integer = 0
    Dim fourth, third, second, first As Integer
    Dim Formulas(175) As String
    Dim Level1 As New List(Of Integer)
    Dim Level2 As New List(Of Integer)

    Private Sub Reset_Click(sender As Object, e As EventArgs) Handles Reset.Click
        ReDim Formulas(175)
        FigureInput.Text = Nothing
        ResultBox.Items.Clear()
        QuizBox.Text = $"List AllとQuiz Modeの{vbNewLine}併用はできません"
        Me.FigureInput.Focus()
        GC.Collect()
    End Sub

    Private Sub RandomButton_Click(sender As Object, e As EventArgs) Handles RandomButton.Click
        If SetN.Checked = True AndAlso (IsNumeric(InputSetN.Text) = False OrElse CInt(InputSetN.Text) <> CSng(InputSetN.Text)) Then
            MessageBox.Show($"nが不正な値です", "エラー")
            InputSetN.Text = Nothing
            Exit Sub
        End If

        Level1.Clear()
        Level2.Clear()

        For i As Integer = 1 To 10000
            Dim RandomNum = New System.Random
            Dim AfterRandom As String = "0000"
            Do While AfterRandom = "0000"
                Dim RandomA As Integer = RandomNum.Next(10)
                Dim RandomB As Integer = RandomNum.Next(10)
                Dim RandomC As Integer = RandomNum.Next(10)
                Dim RandomD As Integer = RandomNum.Next(10)
                AfterRandom = RandomA & RandomB & RandomC & RandomD
            Loop
            FigureInput.Text = AfterRandom
            StartCalc.PerformClick()
            If Level1.Count() <> 0 OrElse Level2.Count() <> 0 Then
                AutoCalcProg.Value = 0
                Exit Sub
            End If
            AutoCalcProg.PerformStep()
        Next
        ResultBox.Items.Add("(乱数生成を10000回行なった結果)")
        ResultBox.Items.Add("List Allを試してください")
        AutoCalcProg.Value = 0
    End Sub

    Private Sub AutoRandom_Click(sender As Object, e As EventArgs) Handles AutoRandom.Click
        If SetN.Checked = True AndAlso (IsNumeric(InputSetN.Text) = False OrElse CInt(InputSetN.Text) <> CSng(InputSetN.Text)) Then
            MessageBox.Show($"nが不正な値です", "エラー")
            InputSetN.Text = Nothing
            Exit Sub
        End If

        Dim RndOrigin As New List(Of String) 'RndOriginはnをつくれる4ケタの数字のリスト
        For i As Integer = 1 To 9999
            Select Case i
                Case < 10
                    FigureInput.Text = "000" & i
                Case < 100
                    FigureInput.Text = "00" & i
                Case < 1000
                    FigureInput.Text = "0" & i
                Case Else
                    FigureInput.Text = i
            End Select
            AutoCalcProg.PerformStep()
            StartCalc.PerformClick()
            If ResultBox.Items.Count > 0 Then
                For j As Integer = 0 To 175
                    If Formulas(j) <> Nothing Then
                        RndOrigin.Add(FigureInput.Text & " → " & Formulas(j))
                    End If
                Next
            End If
        Next

        FigureInput.Text = RndOrigin.Count() & "通り"
        ResultBox.Items.Clear()

        For i As Integer = 0 To RndOrigin.Count
            If RndOrigin.Count = 0 Then
                ResultBox.Items.Add($"{ten}を作れる4ケタの数字の組はありませんでした")
            ElseIf i = RndOrigin.Count Then
                Exit For
            Else
                ResultBox.Items.Add(RndOrigin.Item(i))
            End If
        Next

        QuizBox.Text = $"List AllとQuiz Modeの{vbNewLine}併用はできません"
        AutoCalcProg.Value = 0
        RndOrigin.Clear()
    End Sub

    Private Sub SetTen_Click_1(sender As Object, e As EventArgs) Handles SetTen.Click
        SetN.Checked = False
        InputSetN.Visible = False
    End Sub

    Private Sub SetN_Click_1(sender As Object, e As EventArgs) Handles SetN.Click
        SetTen.Checked = False
        InputSetN.Visible = True
    End Sub

    Private Sub StartCalc_Click(sender As Object, e As EventArgs) Handles StartCalc.Click
        ResultBox.Items.Clear()
        Level1.Clear()
        Level2.Clear()
        QuizBox.Text = Nothing
        ReDim Formulas(175)
        IntOrigin = 0
        StrOrigin = FigureInput.Text
        If IsNumeric(StrOrigin) = True AndAlso CInt(StrOrigin) = CSng(StrOrigin) AndAlso CInt(StrOrigin) > 0 AndAlso CInt("1" + StrOrigin) >= 10000 AndAlso CInt("1" + StrOrigin) <= 19999 Then
            IntOrigin = CInt("1" + StrOrigin)
        Else
            MessageBox.Show("4ケタの自然数を入力してください", "エラー")
            FigureInput.Text = Nothing
            Exit Sub
        End If

        fourth = IntOrigin \ 1000 - 10
        third = IntOrigin \ 100 - (IntOrigin \ 1000) * 10
        second = IntOrigin \ 10 - (IntOrigin \ 100) * 10
        first = IntOrigin - (IntOrigin \ 10) * 10

        If SetN.Checked = True AndAlso IsNumeric(InputSetN.Text) = True AndAlso CInt(InputSetN.Text) = CSng(InputSetN.Text) Then
            ten = InputSetN.Text
        ElseIf SetTen.Checked = True Then
            ten = 10
        Else
            MessageBox.Show("nが不正な値です", "エラー")
            InputSetN.Text = Nothing
            Exit Sub
        End If

        '括弧を用いない64パターンの列挙
        If fourth + third + second + first = ten Then
            Formulas(0) = $"{fourth}+{third}+{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth + third + second - first = ten Then
            Formulas(1) = $"{fourth}+{third}+{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth + third + second * first = ten Then
            Formulas(2) = $"{fourth}+{third}+{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth + third + second / first = ten Then
            Formulas(3) = $"{fourth}+{third}+{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth + third - second + first = ten Then
            Formulas(4) = $"{fourth}+{third}-{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth + third - second - first = ten Then
            Formulas(5) = $"{fourth}+{third}-{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth + third - second * first = ten Then
            Formulas(6) = $"{fourth}+{third}-{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth + third - second / first = ten Then
            Formulas(7) = $"{fourth}+{third}-{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth + third * second + first = ten Then
            Formulas(8) = $"{fourth}+{third}*{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth + third * second - first = ten Then
            Formulas(9) = $"{fourth}+{third}*{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth + third * second * first = ten Then
            Formulas(10) = $"{fourth}+{third}*{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth + third * second / first = ten Then
            Formulas(11) = $"{fourth}+{third}*{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth + third / second + first = ten Then
            Formulas(12) = $"{fourth}+{third}/{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth + third / second - first = ten Then
            Formulas(13) = $"{fourth}+{third}/{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth + third / second * first = ten Then
            Formulas(14) = $"{fourth}+{third}/{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth + third / second / first = ten Then
            Formulas(15) = $"{fourth}+{third}/{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth - third + second + first = ten Then
            Formulas(16) = $"{fourth}-{third}+{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth - third + second - first = ten Then
            Formulas(17) = $"{fourth}-{third}+{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth - third + second * first = ten Then
            Formulas(18) = $"{fourth}-{third}+{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth - third + second / first = ten Then
            Formulas(19) = $"{fourth}-{third}+{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth - third - second + first = ten Then
            Formulas(20) = $"{fourth}-{third}-{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth - third - second - first = ten Then
            Formulas(21) = $"{fourth}-{third}-{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth - third - second * first = ten Then
            Formulas(22) = $"{fourth}-{third}-{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth - third - second / first = ten Then
            Formulas(23) = $"{fourth}-{third}-{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth - third * second + first = ten Then
            Formulas(24) = $"{fourth}-{third}*{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth - third * second - first = ten Then
            Formulas(25) = $"{fourth}-{third}*{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth - third * second * first = ten Then
            Formulas(26) = $"{fourth}-{third}*{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth - third * second / first = ten Then
            Formulas(27) = $"{fourth}-{third}*{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth - third / second + first = ten Then
            Formulas(28) = $"{fourth}-{third}/{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth - third / second - first = ten Then
            Formulas(29) = $"{fourth}-{third}/{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth - third / second * first = ten Then
            Formulas(30) = $"{fourth}-{third}/{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth - third / second / first = ten Then
            Formulas(31) = $"{fourth}-{third}/{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth * third + second + first = ten Then
            Formulas(32) = $"{fourth}*{third}+{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth * third + second - first = ten Then
            Formulas(33) = $"{fourth}*{third}+{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth * third + second * first = ten Then
            Formulas(34) = $"{fourth}*{third}+{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth * third + second / first = ten Then
            Formulas(35) = $"{fourth}*{third}+{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth * third - second + first = ten Then
            Formulas(36) = $"{fourth}*{third}-{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth * third - second - first = ten Then
            Formulas(37) = $"{fourth}*{third}-{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth * third - second * first = ten Then
            Formulas(38) = $"{fourth}*{third}-{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth * third - second / first = ten Then
            Formulas(39) = $"{fourth}*{third}-{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth * third * second + first = ten Then
            Formulas(40) = $"{fourth}*{third}*{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth * third * second - first = ten Then
            Formulas(41) = $"{fourth}*{third}*{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth * third * second * first = ten Then
            Formulas(42) = $"{fourth}*{third}*{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth * third * second / first = ten Then
            Formulas(43) = $"{fourth}*{third}*{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth * third / second + first = ten Then
            Formulas(44) = $"{fourth}*{third}/{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth * third / second - first = ten Then
            Formulas(45) = $"{fourth}*{third}/{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth * third / second * first = ten Then
            Formulas(46) = $"{fourth}*{third}/{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth * third / second / first = ten Then
            Formulas(47) = $"{fourth}*{third}/{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth / third + second + first = ten Then
            Formulas(48) = $"{fourth}/{third}+{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth / third + second - first = ten Then
            Formulas(49) = $"{fourth}/{third}+{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth / third + second * first = ten Then
            Formulas(50) = $"{fourth}/{third}+{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth / third + second / first = ten Then
            Formulas(51) = $"{fourth}/{third}+{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth / third - second + first = ten Then
            Formulas(52) = $"{fourth}/{third}-{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth / third - second - first = ten Then
            Formulas(53) = $"{fourth}/{third}-{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth / third - second * first = ten Then
            Formulas(54) = $"{fourth}/{third}-{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth / third - second / first = ten Then
            Formulas(55) = $"{fourth}/{third}-{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth / third * second + first = ten Then
            Formulas(56) = $"{fourth}/{third}*{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth / third * second - first = ten Then
            Formulas(57) = $"{fourth}/{third}*{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth / third * second * first = ten Then
            Formulas(58) = $"{fourth}/{third}*{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth / third * second / first = ten Then
            Formulas(59) = $"{fourth}/{third}*{second}/{first}={ten}{vbNewLine}"
        End If
        If fourth / third / second + first = ten Then
            Formulas(60) = $"{fourth}/{third}/{second}+{first}={ten}{vbNewLine}"
        End If
        If fourth / third / second - first = ten Then
            Formulas(61) = $"{fourth}/{third}/{second}-{first}={ten}{vbNewLine}"
        End If
        If fourth / third / second * first = ten Then
            Formulas(62) = $"{fourth}/{third}/{second}*{first}={ten}{vbNewLine}"
        End If
        If fourth / third / second / first = ten Then
            Formulas(63) = $"{fourth}/{third}/{second}/{first}={ten}{vbNewLine}"
        End If

        'A(BC)D型
        If fourth * (third + second) * first = ten Then
            Formulas(64) = $"{fourth}*({third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth * (third + second) / first = ten Then
            Formulas(65) = $"{fourth}*({third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth / (third + second) * first = ten Then
            Formulas(66) = $"{fourth}/({third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth / (third + second) / first = ten Then
            Formulas(67) = $"{fourth}/({third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth * (third - second) * first = ten Then
            Formulas(68) = $"{fourth}*({third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth * (third - second) / first = ten Then
            Formulas(69) = $"{fourth}*({third}-{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth / (third - second) * first = ten Then
            Formulas(70) = $"{fourth}/({third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth / (third - second) / first = ten Then
            Formulas(71) = $"{fourth}/({third}-{second})/{first}={ten}{vbNewLine}"
        End If

        '(AB)CD型
        If (fourth + third) * second * first = ten Then
            Formulas(72) = $"({fourth}+{third})*{second}*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) * second / first = ten Then
            Formulas(73) = $"({fourth}+{third})*{second}/{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) / second * first = ten Then
            Formulas(74) = $"({fourth}+{third})/{second}*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) / second / first = ten Then
            Formulas(75) = $"({fourth}+{third})/{second}/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) * second * first = ten Then
            Formulas(76) = $"({fourth}-{third})*{second}*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) * second / first = ten Then
            Formulas(77) = $"({fourth}-{third})*{second}/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) / second * first = ten Then
            Formulas(78) = $"({fourth}-{third})/{second}*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) / second / first = ten Then
            Formulas(79) = $"({fourth}-{third})/{second}/{first}={ten}{vbNewLine}"
        End If

        'AB(CD)型
        If fourth * third * (second + first) = ten Then
            Formulas(80) = $"{fourth}*{third}*({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * third / (second + first) = ten Then
            Formulas(81) = $"{fourth}*{third}/({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / third * (second + first) = ten Then
            Formulas(82) = $"{fourth}/{third}*({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / third / (second + first) = ten Then
            Formulas(83) = $"{fourth}/{third}/({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * third * (second - first) = ten Then
            Formulas(84) = $"{fourth}*{third}*({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth * third / (second - first) = ten Then
            Formulas(85) = $"{fourth}*{third}/({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / third * (second - first) = ten Then
            Formulas(86) = $"{fourth}/{third}*({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / third / (second - first) = ten Then
            Formulas(87) = $"{fourth}/{third}/({second}-{first})={ten}{vbNewLine}"
        End If

        '(AB)(CD)型
        If (fourth + third) * (second + first) = ten Then
            Formulas(88) = $"({fourth}+{third})*({second}+{first})={ten}{vbNewLine}"
        End If
        If (fourth + third) * (second - first) = ten Then
            Formulas(89) = $"({fourth}+{third})*({second}-{first})={ten}{vbNewLine}"
        End If
        If (fourth - third) * (second + first) = ten Then
            Formulas(90) = $"({fourth}-{third})*({second}+{first})={ten}{vbNewLine}"
        End If
        If (fourth - third) * (second - first) = ten Then
            Formulas(91) = $"({fourth}-{third})*({second}-{first})={ten}{vbNewLine}"
        End If
        If (fourth + third) / (second + first) = ten Then
            Formulas(92) = $"({fourth}+{third})/({second}+{first})={ten}{vbNewLine}"
        End If
        If (fourth + third) / (second - first) = ten Then
            Formulas(93) = $"({fourth}+{third})/({second}-{first})={ten}{vbNewLine}"
        End If
        If (fourth - third) / (second + first) = ten Then
            Formulas(94) = $"({fourth}-{third})/({second}+{first})={ten}{vbNewLine}"
        End If
        If (fourth - third) / (second - first) = ten Then
            Formulas(95) = $"({fourth}-{third})/({second}-{first})={ten}{vbNewLine}"
        End If

        '(A+B+C)D型
        If (fourth + third + second) * first = ten Then
            Formulas(96) = $"({fourth}+{third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third - second) * first = ten Then
            Formulas(97) = $"({fourth}+{third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third + second) * first = ten Then
            Formulas(98) = $"({fourth}-{third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third - second) * first = ten Then
            Formulas(99) = $"({fourth}-{third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third + second) / first = ten Then
            Formulas(100) = $"({fourth}+{third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth + third - second) / first = ten Then
            Formulas(101) = $"({fourth}+{third}-{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third + second) / first = ten Then
            Formulas(102) = $"({fourth}-{third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third - second) / first = ten Then
            Formulas(103) = $"({fourth}-{third}-{second})/{first}={ten}{vbNewLine}"
        End If

        '(A+B*C)D型
        If (fourth + third * second) * first = ten Then
            Formulas(104) = $"({fourth}+{third}*{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third * second) * first = ten Then
            Formulas(105) = $"({fourth}-{third}*{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third * second) / first = ten Then
            Formulas(106) = $"({fourth}+{third}*{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third * second) / first = ten Then
            Formulas(107) = $"({fourth}-{third}*{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth + third / second) * first = ten Then
            Formulas(108) = $"({fourth}+{third}/{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth - third / second) * first = ten Then
            Formulas(109) = $"({fourth}-{third}/{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth + third / second) / first = ten Then
            Formulas(110) = $"({fourth}+{third}/{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth - third / second) / first = ten Then
            Formulas(111) = $"({fourth}-{third}/{second})/{first}={ten}{vbNewLine}"
        End If

        '(A*B+C)D型
        If (fourth * third + second) * first = ten Then
            Formulas(112) = $"({fourth}*{third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth * third - second) * first = ten Then
            Formulas(113) = $"({fourth}*{third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth * third + second) / first = ten Then
            Formulas(114) = $"({fourth}*{third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth * third - second) / first = ten Then
            Formulas(115) = $"({fourth}*{third}-{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth / third + second) * first = ten Then
            Formulas(116) = $"({fourth}/{third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth / third - second) * first = ten Then
            Formulas(117) = $"({fourth}/{third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If (fourth / third + second) / first = ten Then
            Formulas(118) = $"({fourth}/{third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If (fourth / third - second) / first = ten Then
            Formulas(119) = $"({fourth}/{third}-{second})/{first}={ten}{vbNewLine}"
        End If

        'A(B+C+D)型
        If fourth * (third + second + first) = ten Then
            Formulas(120) = $"{fourth}*({third}+{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * (third + second - first) = ten Then
            Formulas(121) = $"{fourth}*({third}+{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth * (third - second + first) = ten Then
            Formulas(122) = $"{fourth}*({third}-{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * (third - second - first) = ten Then
            Formulas(123) = $"{fourth}*({third}-{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / (third + second + first) = ten Then
            Formulas(124) = $"{fourth}/({third}+{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / (third + second - first) = ten Then
            Formulas(125) = $"{fourth}/({third}+{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / (third - second + first) = ten Then
            Formulas(126) = $"{fourth}/({third}-{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / (third - second - first) = ten Then
            Formulas(127) = $"{fourth}/({third}-{second}-{first})={ten}{vbNewLine}"
        End If

        'A(B+C*D)型
        If fourth * (third + second * first) = ten Then
            Formulas(128) = $"{fourth}*({third}+{second}*{first})={ten}{vbNewLine}"
        End If
        If fourth * (third + second / first) = ten Then
            Formulas(129) = $"{fourth}*({third}+{second}/{first})={ten}{vbNewLine}"
        End If
        If fourth * (third - second * first) = ten Then
            Formulas(130) = $"{fourth}*({third}-{second}*{first})={ten}{vbNewLine}"
        End If
        If fourth * (third - second / first) = ten Then
            Formulas(131) = $"{fourth}*({third}-{second}/{first})={ten}{vbNewLine}"
        End If
        If fourth / (third + second * first) = ten Then
            Formulas(132) = $"{fourth}/({third}+{second}*{first})={ten}{vbNewLine}"
        End If
        If fourth / (third + second / first) = ten Then
            Formulas(133) = $"{fourth}/({third}+{second}/{first})={ten}{vbNewLine}"
        End If
        If fourth / (third - second * first) = ten Then
            Formulas(134) = $"{fourth}/({third}-{second}*{first})={ten}{vbNewLine}"
        End If
        If fourth / (third - second / first) = ten Then
            Formulas(135) = $"{fourth}/({third}-{second}/{first})={ten}{vbNewLine}"
        End If

        'A(B*C+D)型
        If fourth * (third * second + first) = ten Then
            Formulas(136) = $"{fourth}*({third}*{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * (third * second - first) = ten Then
            Formulas(137) = $"{fourth}*({third}*{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth * (third / second + first) = ten Then
            Formulas(138) = $"{fourth}*({third}/{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth * (third / second - first) = ten Then
            Formulas(139) = $"{fourth}*({third}/{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / (third * second + first) = ten Then
            Formulas(140) = $"{fourth}/({third}*{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / (third * second - first) = ten Then
            Formulas(141) = $"{fourth}/({third}*{second}-{first})={ten}{vbNewLine}"
        End If
        If fourth / (third / second + first) = ten Then
            Formulas(142) = $"{fourth}/({third}/{second}+{first})={ten}{vbNewLine}"
        End If
        If fourth / (third / second - first) = ten Then
            Formulas(143) = $"{fourth}/({third}/{second}-{first})={ten}{vbNewLine}"
        End If

        '(AB)C+D型
        If (fourth + third) * second + first = ten Then
            Formulas(144) = $"({fourth}+{third})*{second}+{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) * second - first = ten Then
            Formulas(145) = $"({fourth}+{third})*{second}-{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) / second + first = ten Then
            Formulas(146) = $"({fourth}+{third})/{second}+{first}={ten}{vbNewLine}"
        End If
        If (fourth + third) / second - first = ten Then
            Formulas(147) = $"({fourth}+{third})/{second}-{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) * second + first = ten Then
            Formulas(148) = $"({fourth}-{third})*{second}+{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) * second - first = ten Then
            Formulas(149) = $"({fourth}-{third})*{second}-{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) / second + first = ten Then
            Formulas(150) = $"({fourth}-{third})/{second}+{first}={ten}{vbNewLine}"
        End If
        If (fourth - third) / second - first = ten Then
            Formulas(151) = $"({fourth}-{third})/{second}-{first}={ten}{vbNewLine}"
        End If

        'A+B(CD)型
        If fourth + third * (second + first) = ten Then
            Formulas(152) = $"{fourth}+{third}*({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth + third / (second + first) = ten Then
            Formulas(153) = $"{fourth}+{third}/({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth - third * (second + first) = ten Then
            Formulas(154) = $"{fourth}-{third}*({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth - third / (second + first) = ten Then
            Formulas(155) = $"{fourth}-{third}/({second}+{first})={ten}{vbNewLine}"
        End If
        If fourth + third * (second - first) = ten Then
            Formulas(156) = $"{fourth}+{third}*({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth + third / (second - first) = ten Then
            Formulas(157) = $"{fourth}+{third}/({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth - third * (second - first) = ten Then
            Formulas(158) = $"{fourth}-{third}*({second}-{first})={ten}{vbNewLine}"
        End If
        If fourth - third / (second - first) = ten Then
            Formulas(159) = $"{fourth}-{third}/({second}-{first})={ten}{vbNewLine}"
        End If

        'A+(BC)*D型
        If fourth + (third + second) * first = ten Then
            Formulas(160) = $"{fourth}+({third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth + (third + second) / first = ten Then
            Formulas(161) = $"{fourth}+({third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth - (third + second) * first = ten Then
            Formulas(162) = $"{fourth}-({third}+{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth - (third + second) / first = ten Then
            Formulas(163) = $"{fourth}-({third}+{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth + (third - second) * first = ten Then
            Formulas(164) = $"{fourth}+({third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth + (third - second) / first = ten Then
            Formulas(165) = $"{fourth}+({third}-{second})/{first}={ten}{vbNewLine}"
        End If
        If fourth - (third - second) * first = ten Then
            Formulas(166) = $"{fourth}-({third}-{second})*{first}={ten}{vbNewLine}"
        End If
        If fourth - (third - second) / first = ten Then
            Formulas(167) = $"{fourth}-({third}-{second})/{first}={ten}{vbNewLine}"
        End If

        'A*(BC)+D型
        If fourth * (third + second) + first = ten Then
            Formulas(168) = $"{fourth}*({third}+{second})+{first}={ten}{vbNewLine}"
        End If
        If fourth * (third + second) - first = ten Then
            Formulas(169) = $"{fourth}*({third}+{second})-{first}={ten}{vbNewLine}"
        End If
        If fourth / (third + second) + first = ten Then
            Formulas(170) = $"{fourth}/({third}+{second})+{first}={ten}{vbNewLine}"
        End If
        If fourth / (third + second) - first = ten Then
            Formulas(171) = $"{fourth}/({third}+{second})-{first}={ten}{vbNewLine}"
        End If
        If fourth * (third - second) + first = ten Then
            Formulas(172) = $"{fourth}*({third}-{second})+{first}={ten}{vbNewLine}"
        End If
        If fourth * (third - second) - first = ten Then
            Formulas(173) = $"{fourth}*({third}-{second})-{first}={ten}{vbNewLine}"
        End If
        If fourth / (third - second) + first = ten Then
            Formulas(174) = $"{fourth}/({third}-{second})+{first}={ten}{vbNewLine}"
        End If
        If fourth / (third - second) - first = ten Then
            Formulas(175) = $"{fourth}/({third}-{second})-{first}={ten}{vbNewLine}"
        End If

        For i As Integer = 0 To 175
            If Formulas(i) <> Nothing Then
                ResultBox.Items.Add(Formulas(i))
                Select Case i
                    Case <= 63
                        Level1.Add(i)
                    Case >= 64
                        Level2.Add(i)
                End Select
            End If
        Next

        If ResultBox.Items.Count = 0 Then
            ResultBox.Items.Add($"{ten}を作れませんでした")
        End If

        QuizBox.Text = $"{FigureInput.Text}を使って{ten}を作る式のうち{vbNewLine}括弧を用いない式が{Level1.Count()}個{vbNewLine}括弧を用いる式が{Level2.Count()}個あります"

    End Sub

    Private Sub SelectQuiz_Click(sender As Object, e As EventArgs) Handles SelectQuiz.Click
        Select Case SelectQuiz.Checked
            Case True
                QuizBox.Visible = True
                AutoRandom.Enabled = False
                If QuizBox.Text = Nothing Then
                    QuizBox.Text = $"List AllとQuiz Modeの{vbNewLine}併用はできません"
                End If
            Case False
                QuizBox.Visible = False
                AutoRandom.Enabled = True
        End Select
    End Sub

End Class
