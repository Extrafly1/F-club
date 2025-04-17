open System
open System.Drawing
open System.Windows.Forms

// Задание 1: Главная форма с меню
let mainForm = new Form(Text = "Главная форма", Width = 600, Height = 400)
let menu = new MainMenu()
let menuItem = menu.MenuItems.Add("Задания")

let addTaskMenu text handler =
    let item = new MenuItem(Text = text)
    item.Click.Add(handler)
    menuItem.MenuItems.Add(item) |> ignore

// Задание 2: Решение квадратного уравнения
let createTask2Form() =
    let form = new Form(Text = "Квадратное уравнение", Width = 400, Height = 300)
    let tbA = new TextBox(Location = Point(20, 20), Width = 100)
    let tbB = new TextBox(Location = Point(20, 50), Width = 100)
    let tbC = new TextBox(Location = Point(20, 80), Width = 100)
    let btn = new Button(Text = "Решить", Location = Point(20, 110), Width = 100)
    let rtb = new RichTextBox(Location = Point(20, 150), Size = Size(340, 100))
    
    btn.Click.Add(fun _ ->
        try
            let a = float tbA.Text
            let b = float tbB.Text
            let c = float tbC.Text
            let discr = b * b - 4.0 * a * c
            rtb.Text <- sprintf "Дискриминант: %0.2f\n" discr +
                match a, discr with
                | 0., _ -> "Не квадратное уравнение!"
                | _, d when d < 0.0 -> "Нет действительных корней"
                | _, d when d = 0.0 -> sprintf "Один корень: %0.2f" (-b/(2.0*a))
                | _ -> sprintf "Два корня: %0.2f и %0.2f" ((-b + sqrt discr)/(2.0*a)) ((-b - sqrt discr)/(2.0*a))
        with _ -> rtb.Text <- "Ошибка ввода!")
    form.Controls.AddRange([|tbA; tbB; tbC; btn; rtb|])
    form

// Задание 3: Калькулятор
let createTask3Form() =
    let form = new Form(Text = "Калькулятор", Width = 300, Height = 200)
    let tb1 = new TextBox(Location = Point(20, 20), Width = 100)
    let tb2 = new TextBox(Location = Point(20, 50), Width = 100)
    let cb = new ComboBox(Location = Point(20, 80), Width = 100, DropDownStyle = ComboBoxStyle.DropDownList)
    cb.Items.AddRange([|"+"; "-"; "*"; "/"|] |> Array.map box)
    cb.SelectedIndex <- 0
    let btn = new Button(Text = "=", Location = Point(20, 110), Width = 100)
    let lbl = new Label(Location = Point(20, 140), Width = 100)
    
    btn.Click.Add(fun _ ->
        try
            let x = float tb1.Text
            let y = float tb2.Text
            lbl.Text <-
                match cb.SelectedItem.ToString() with
                | "+" -> sprintf "%0.2f" (x + y)
                | "-" -> sprintf "%0.2f" (x - y)
                | "*" -> sprintf "%0.2f" (x * y)
                | "/" -> if y <> 0.0 then sprintf "%0.2f" (x / y) else "Деление на ноль!"
                | _ -> "Ошибка"
        with _ -> lbl.Text <- "Ошибка!")
    form.Controls.AddRange([|tb1; tb2; cb; btn; lbl|])
    form

// Задание 4: Смена изображения
let createTask4Form() =
    let form = new Form(Text = "Смена изображения", Width = 300, Height = 300)
    let pb = new PictureBox(Size = Size(200, 200), Location = Point(50, 20), SizeMode = PictureBoxSizeMode.StretchImage)
    let btn = new Button(Text = "Сменить", Location = Point(100, 240))
    let images = [|SystemIcons.Information; SystemIcons.Warning; SystemIcons.Error|]
    let mutable index = 0
    
    btn.Click.Add(fun _ ->
        index <- (index + 1) % images.Length
        pb.Image <- images.[index].ToBitmap())
    pb.Image <- images.[0].ToBitmap()
    form.Controls.AddRange([|pb; btn|])
    form

// Задание 5: Времена года
let createTask5Form() =
    let form = new Form(Text = "Времена года", Width = 300, Height = 200)
    let cb = new ComboBox(Location = Point(20, 20), Width = 100, DropDownStyle = ComboBoxStyle.DropDownList)
    cb.Items.AddRange([|"Январь"; "Февраль"; "Март"; "Апрель"; "Май"; "Июнь";
                      "Июль"; "Август"; "Сентябрь"; "Октябрь"; "Ноябрь"; "Декабрь"|])
    cb.SelectedIndex <- 0
    let btn = new Button(Text = "Показать", Location = Point(20, 50), Width = 100)
    let lbl = new Label(Location = Point(20, 80), Width = 200)
    
    btn.Click.Add(fun _ ->
        let season =
            match cb.SelectedIndex with
            | 11 | 0 | 1 -> "Зима"
            | 2 | 3 | 4 -> "Весна"
            | 5 | 6 | 7 -> "Лето"
            | _ -> "Осень"
        lbl.Text <- sprintf "%s - %s" (cb.SelectedItem.ToString()) season)
    form.Controls.AddRange([|cb; btn; lbl|])
    form

// Задание 6: Изменение ширины кнопки
let createTask6Form() =
    let form = new Form(Text = "Изменение ширины", Width = 300, Height = 200)
    let trackBar = new TrackBar(Location = Point(20, 20), Minimum = 50, Maximum = 200, Value = 100)
    let btn = new Button(Text = "Кнопка", Location = Point(20, 60), Height = 30, Width = 100)
    
    trackBar.ValueChanged.Add(fun _ -> btn.Width <- trackBar.Value)
    form.Controls.AddRange([|trackBar; btn|])
    form

// Задание 7: Флажки
let createTask7Form() =
    let form = new Form(Text = "Флажки", Width = 300, Height = 200)
    let chk1 = new CheckBox(Text = "Первый", Location = Point(20, 20))
    let chk2 = new CheckBox(Text = "Второй", Location = Point(20, 50))
    let btn = new Button(Text = "Проверить", Location = Point(20, 80))
    
    btn.Click.Add(fun _ ->
        let state = 
            match chk1.Checked, chk2.Checked with
            | true, true -> "Оба флажка"
            | true, false -> "Первый флажок"
            | false, true -> "Второй флажок"
            | _ -> "Ничего"
        MessageBox.Show(sprintf "Установлен: %s" state) |> ignore)
    form.Controls.AddRange([|chk1; chk2; btn|])
    form

// Задание 8: Добавление в список
let createTask8Form() =
    let form = new Form(Text = "Добавление в список", Width = 300, Height = 300)
    let tb = new TextBox(Location = Point(20, 20), Width = 100)
    let btn = new Button(Text = "Добавить", Location = Point(130, 20))
    let listBox = new ListBox(Location = Point(20, 60), Width = 200, Height = 150)
    
    btn.Click.Add(fun _ ->
        if not (String.IsNullOrWhiteSpace(tb.Text)) then
            listBox.Items.Add(tb.Text) |> ignore
            tb.Clear())
    form.Controls.AddRange([|tb; btn; listBox|])
    form

// Задание 9: Индикатор загрузки
let createTask9Form() =
    let form = new Form(Text = "Индикатор загрузки", Width = 300, Height = 200)
    let tb = new TextBox(Location = Point(20, 20), Width = 200)
    let progress = new ProgressBar(Location = Point(20, 50), Width = 200, Maximum = 100)
    
    tb.TextChanged.Add(fun _ ->
        progress.Value <- min (tb.Text.Length * 10) 100)
    form.Controls.AddRange([|tb; progress|])
    form

// Задание 10: Площадь прямоугольника
let createTask10Form() =
    let form = new Form(Text = "Площадь прямоугольника", Width = 300, Height = 200)
    let tb1 = new TextBox(Location = Point(20, 20), Width = 100)
    let tb2 = new TextBox(Location = Point(20, 50), Width = 100)
    let btn = new Button(Text = "Вычислить", Location = Point(20, 80), Width = 100)
    let lbl = new Label(Location = Point(20, 110), Width = 200)
    
    btn.Click.Add(fun _ ->
        try
            let a = float tb1.Text
            let b = float tb2.Text
            lbl.Text <- sprintf "Площадь: %0.2f" (a * b)
        with _ -> lbl.Text <- "Ошибка ввода!")
    form.Controls.AddRange([|tb1; tb2; btn; lbl|])
    form

// Меню
[<STAThread>]
do
    addTaskMenu "Квадратное уравнение" (fun _ -> createTask2Form().ShowDialog() |> ignore)
    addTaskMenu "Калькулятор" (fun _ -> createTask3Form().ShowDialog() |> ignore)
    addTaskMenu "Смена изображения" (fun _ -> createTask4Form().ShowDialog() |> ignore)
    addTaskMenu "Времена года" (fun _ -> createTask5Form().ShowDialog() |> ignore)
    addTaskMenu "Изменение ширины" (fun _ -> createTask6Form().ShowDialog() |> ignore)
    addTaskMenu "Флажки" (fun _ -> createTask7Form().ShowDialog() |> ignore)
    addTaskMenu "Добавление в список" (fun _ -> createTask8Form().ShowDialog() |> ignore)
    addTaskMenu "Индикатор загрузки" (fun _ -> createTask9Form().ShowDialog() |> ignore)
    addTaskMenu "Площадь прямоугольника" (fun _ -> createTask10Form().ShowDialog() |> ignore)

    mainForm.Menu <- menu
    Application.EnableVisualStyles()
    Application.Run(mainForm)
