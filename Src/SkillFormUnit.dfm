object SkillForm: TSkillForm
  Left = 0
  Top = 0
  Caption = 'Skill'
  ClientHeight = 603
  ClientWidth = 920
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnActivate = FormActivate
  OnClick = FormClick
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 608
    Top = 20
    Width = 105
    Height = 16
    Caption = 'HotKeys  Settigs'
  end
  object Label2: TLabel
    Left = 168
    Top = 20
    Width = 44
    Height = 16
    Caption = 'Use List'
  end
  object SkillGrid: TAdvStringGrid
    AlignWithMargins = True
    Left = 20
    Top = 40
    Width = 413
    Height = 553
    Cursor = crDefault
    Margins.Left = 20
    Margins.Top = 40
    Margins.Right = 0
    Margins.Bottom = 10
    Align = alLeft
    DefaultRowHeight = 38
    DrawingStyle = gdsClassic
    RowCount = 1
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    HoverRowCells = [hcNormal, hcSelected]
    ActiveCellFont.Charset = DEFAULT_CHARSET
    ActiveCellFont.Color = clWindowText
    ActiveCellFont.Height = -11
    ActiveCellFont.Name = 'Tahoma'
    ActiveCellFont.Style = [fsBold]
    ControlLook.FixedGradientHoverFrom = clGray
    ControlLook.FixedGradientHoverTo = clWhite
    ControlLook.FixedGradientDownFrom = clGray
    ControlLook.FixedGradientDownTo = clSilver
    ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownHeader.Font.Color = clWindowText
    ControlLook.DropDownHeader.Font.Height = -11
    ControlLook.DropDownHeader.Font.Name = 'Tahoma'
    ControlLook.DropDownHeader.Font.Style = []
    ControlLook.DropDownHeader.Visible = True
    ControlLook.DropDownHeader.Buttons = <>
    ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownFooter.Font.Color = clWindowText
    ControlLook.DropDownFooter.Font.Height = -11
    ControlLook.DropDownFooter.Font.Name = 'Tahoma'
    ControlLook.DropDownFooter.Font.Style = []
    ControlLook.DropDownFooter.Visible = True
    ControlLook.DropDownFooter.Buttons = <>
    Filter = <>
    FilterDropDown.Font.Charset = DEFAULT_CHARSET
    FilterDropDown.Font.Color = clWindowText
    FilterDropDown.Font.Height = -11
    FilterDropDown.Font.Name = 'Tahoma'
    FilterDropDown.Font.Style = []
    FilterDropDown.TextChecked = 'Checked'
    FilterDropDown.TextUnChecked = 'Unchecked'
    FilterDropDownClear = '(All)'
    FilterEdit.TypeNames.Strings = (
      'Starts with'
      'Ends with'
      'Contains'
      'Not contains'
      'Equal'
      'Not equal'
      'Larger than'
      'Smaller than'
      'Clear')
    FixedColWidth = 10
    FixedRowHeight = 22
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = [fsBold]
    FloatFormat = '%.2f'
    HoverButtons.Buttons = <>
    HoverButtons.Position = hbLeftFromColumnLeft
    HTMLSettings.ImageFolder = 'images'
    HTMLSettings.ImageBaseName = 'img'
    PrintSettings.DateFormat = 'dd/mm/yyyy'
    PrintSettings.Font.Charset = DEFAULT_CHARSET
    PrintSettings.Font.Color = clWindowText
    PrintSettings.Font.Height = -11
    PrintSettings.Font.Name = 'Tahoma'
    PrintSettings.Font.Style = []
    PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
    PrintSettings.FixedFont.Color = clWindowText
    PrintSettings.FixedFont.Height = -11
    PrintSettings.FixedFont.Name = 'Tahoma'
    PrintSettings.FixedFont.Style = []
    PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
    PrintSettings.HeaderFont.Color = clWindowText
    PrintSettings.HeaderFont.Height = -11
    PrintSettings.HeaderFont.Name = 'Tahoma'
    PrintSettings.HeaderFont.Style = []
    PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
    PrintSettings.FooterFont.Color = clWindowText
    PrintSettings.FooterFont.Height = -11
    PrintSettings.FooterFont.Name = 'Tahoma'
    PrintSettings.FooterFont.Style = []
    PrintSettings.PageNumSep = '/'
    ScrollBarAlways = saVert
    ScrollSynch = True
    ScrollWidth = 10
    SearchFooter.FindNextCaption = 'Find &next'
    SearchFooter.FindPrevCaption = 'Find &previous'
    SearchFooter.Font.Charset = DEFAULT_CHARSET
    SearchFooter.Font.Color = clWindowText
    SearchFooter.Font.Height = -11
    SearchFooter.Font.Name = 'Tahoma'
    SearchFooter.Font.Style = []
    SearchFooter.HighLightCaption = 'Highlight'
    SearchFooter.HintClose = 'Close'
    SearchFooter.HintFindNext = 'Find next occurence'
    SearchFooter.HintFindPrev = 'Find previous occurence'
    SearchFooter.HintHighlight = 'Highlight occurences'
    SearchFooter.MatchCaseCaption = 'Match case'
    SearchFooter.ResultFormat = '(%d of %d)'
    SelectionColor = clBtnHighlight
    ShowDesignHelper = False
    SortSettings.DefaultFormat = ssAutomatic
    Version = '8.2.4.1'
    ColWidths = (
      10
      98
      61
      148
      64)
    RowHeights = (
      26)
  end
  object SelectGrid: TAdvStringGrid
    AlignWithMargins = True
    Left = 463
    Top = 40
    Width = 434
    Height = 553
    Cursor = crDefault
    Margins.Left = 30
    Margins.Top = 40
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alLeft
    DefaultRowHeight = 40
    DrawingStyle = gdsClassic
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    HoverRowCells = [hcNormal, hcSelected]
    OnClickCell = SelectGridClickCell
    ActiveCellFont.Charset = DEFAULT_CHARSET
    ActiveCellFont.Color = clWindowText
    ActiveCellFont.Height = -11
    ActiveCellFont.Name = 'Tahoma'
    ActiveCellFont.Style = [fsBold]
    ControlLook.FixedGradientHoverFrom = clGray
    ControlLook.FixedGradientHoverTo = clWhite
    ControlLook.FixedGradientDownFrom = clGray
    ControlLook.FixedGradientDownTo = clSilver
    ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownHeader.Font.Color = clWindowText
    ControlLook.DropDownHeader.Font.Height = -11
    ControlLook.DropDownHeader.Font.Name = 'Tahoma'
    ControlLook.DropDownHeader.Font.Style = []
    ControlLook.DropDownHeader.Visible = True
    ControlLook.DropDownHeader.Buttons = <>
    ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
    ControlLook.DropDownFooter.Font.Color = clWindowText
    ControlLook.DropDownFooter.Font.Height = -11
    ControlLook.DropDownFooter.Font.Name = 'Tahoma'
    ControlLook.DropDownFooter.Font.Style = []
    ControlLook.DropDownFooter.Visible = True
    ControlLook.DropDownFooter.Buttons = <>
    Filter = <>
    FilterDropDown.Font.Charset = DEFAULT_CHARSET
    FilterDropDown.Font.Color = clWindowText
    FilterDropDown.Font.Height = -11
    FilterDropDown.Font.Name = 'Tahoma'
    FilterDropDown.Font.Style = []
    FilterDropDown.TextChecked = 'Checked'
    FilterDropDown.TextUnChecked = 'Unchecked'
    FilterDropDownClear = '(All)'
    FilterEdit.TypeNames.Strings = (
      'Starts with'
      'Ends with'
      'Contains'
      'Not contains'
      'Equal'
      'Not equal'
      'Larger than'
      'Smaller than'
      'Clear')
    FixedColWidth = 10
    FixedRowHeight = 40
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = [fsBold]
    FloatFormat = '%.2f'
    HoverButtons.Buttons = <>
    HoverButtons.Position = hbLeftFromColumnLeft
    HTMLSettings.ImageFolder = 'images'
    HTMLSettings.ImageBaseName = 'img'
    PrintSettings.DateFormat = 'dd/mm/yyyy'
    PrintSettings.Font.Charset = DEFAULT_CHARSET
    PrintSettings.Font.Color = clWindowText
    PrintSettings.Font.Height = -11
    PrintSettings.Font.Name = 'Tahoma'
    PrintSettings.Font.Style = []
    PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
    PrintSettings.FixedFont.Color = clWindowText
    PrintSettings.FixedFont.Height = -11
    PrintSettings.FixedFont.Name = 'Tahoma'
    PrintSettings.FixedFont.Style = []
    PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
    PrintSettings.HeaderFont.Color = clWindowText
    PrintSettings.HeaderFont.Height = -11
    PrintSettings.HeaderFont.Name = 'Tahoma'
    PrintSettings.HeaderFont.Style = []
    PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
    PrintSettings.FooterFont.Color = clWindowText
    PrintSettings.FooterFont.Height = -11
    PrintSettings.FooterFont.Name = 'Tahoma'
    PrintSettings.FooterFont.Style = []
    PrintSettings.PageNumSep = '/'
    ScrollBarAlways = saVert
    ScrollSynch = True
    ScrollWidth = 10
    SearchFooter.FindNextCaption = 'Find &next'
    SearchFooter.FindPrevCaption = 'Find &previous'
    SearchFooter.Font.Charset = DEFAULT_CHARSET
    SearchFooter.Font.Color = clWindowText
    SearchFooter.Font.Height = -11
    SearchFooter.Font.Name = 'Tahoma'
    SearchFooter.Font.Style = []
    SearchFooter.HighLightCaption = 'Highlight'
    SearchFooter.HintClose = 'Close'
    SearchFooter.HintFindNext = 'Find next occurence'
    SearchFooter.HintFindPrev = 'Find previous occurence'
    SearchFooter.HintHighlight = 'Highlight occurences'
    SearchFooter.MatchCaseCaption = 'Match case'
    SearchFooter.ResultFormat = '(%d of %d)'
    SelectionColor = clGradientInactiveCaption
    ShowDesignHelper = False
    SortSettings.DefaultFormat = ssAutomatic
    Version = '8.2.4.1'
    ColWidths = (
      10
      96
      45
      159
      79)
    RowHeights = (
      40
      40
      40
      40
      40
      40
      40
      40
      40
      40)
    object ComBobox1: TscAdvancedComboEdit
      Left = 312
      Top = 7
      Width = 73
      Height = 26
      UseFontColorToStyleColor = False
      ContentMarginLeft = 0
      ContentMarginRight = 0
      ContentMarginTop = 0
      ContentMarginBottom = 0
      CustomBackgroundImageNormalIndex = -1
      CustomBackgroundImageHotIndex = -1
      CustomBackgroundImageDisabledIndex = -1
      PromptTextColor = clNone
      WallpaperIndex = -1
      LeftButton.ComboButton = False
      LeftButton.Enabled = True
      LeftButton.Visible = False
      LeftButton.ShowHint = False
      LeftButton.ShowEllipses = False
      LeftButton.StyleKind = scbsPushButton
      LeftButton.Width = 18
      LeftButton.ImageIndex = -1
      LeftButton.ImageHotIndex = -1
      LeftButton.ImagePressedIndex = -1
      LeftButton.RepeatClick = False
      LeftButton.RepeatClickInterval = 200
      LeftButton.CustomImageNormalIndex = -1
      LeftButton.CustomImageHotIndex = -1
      LeftButton.CustomImagePressedIndex = -1
      LeftButton.CustomImageDisabledIndex = -1
      RightButton.ComboButton = True
      RightButton.Enabled = True
      RightButton.Visible = True
      RightButton.ShowHint = False
      RightButton.ShowEllipses = False
      RightButton.StyleKind = scbsPushButton
      RightButton.Width = 19
      RightButton.ImageIndex = -1
      RightButton.ImageHotIndex = -1
      RightButton.ImagePressedIndex = -1
      RightButton.RepeatClick = False
      RightButton.RepeatClickInterval = 200
      RightButton.CustomImageNormalIndex = -1
      RightButton.CustomImageHotIndex = -1
      RightButton.CustomImagePressedIndex = -1
      RightButton.CustomImageDisabledIndex = -1
      Transparent = False
      BorderKind = scebColorFrame2
      FrameColor = clBtnShadow
      FrameActiveColor = clHighlight
      AlternateRow = False
      ListBoxIndentMargin = 10
      ListBoxHeaderUseStyleColor = True
      ListBoxWallpaperIndex = -1
      ListBoxLineColor = clBtnFace
      ListBoxHeaderStyle = scahsDefault
      ListBoxWidth = 0
      ListBoxHeight = 0
      ListBoxSelectionStyle = scastStyled
      ListBoxShowItemTitles = True
      ListBoxShowItemDetails = False
      ListBoxShowLines = False
      ListBoxItemHeight = 20
      ListBoxHeaderHeight = 20
      ListBoxSelectionColor = clNone
      ListBoxSelectionTextColor = clHighlightText
      UseFilter = False
      Enabled = False
      TabOrder = 4
      Items = <
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'insert'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'home'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'PgUp'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'delete'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'end'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'PgDn'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'A'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'B'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'C'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'D'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'E'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'F'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'G'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'H'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'I'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'J'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'K'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'L'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'M'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'N'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'O'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'P'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'Q'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'R'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'S'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'T'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'U'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'V'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'W'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'X'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'Y'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Caption = 'Z'
          Checked = False
        end
        item
          Button.Enabled = True
          Button.Visible = False
          Button.StyleKind = sclbsPushButton
          Button.Width = 50
          Button.Height = 25
          Button.ImageIndex = -1
          Button.ImageHotIndex = -1
          Button.ImagePressedIndex = -1
          ProgressBar.Min = 0
          ProgressBar.Max = 100
          ProgressBar.Value = 0
          Header = False
          Enabled = True
          Checked = False
        end>
      ItemIndex = -1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = [fsBold]
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -13
      HeaderFont.Name = 'Tahoma'
      HeaderFont.Style = [fsBold]
      DetailFont.Charset = DEFAULT_CHARSET
      DetailFont.Color = clGrayText
      DetailFont.Height = -13
      DetailFont.Name = 'Tahoma'
      DetailFont.Style = []
      DropDownCount = 25
      Text = ''
      AutoSelect = False
      ReadOnly = True
      OnCloseUp = ComBobox1CloseUp
    end
  end
end
