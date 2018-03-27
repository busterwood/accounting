namespace Accounting

open System 

module Program =
    let income = ???m

    let CorporationTax amount = amount * 0.2m
    let incomeTaxAllowance income = if income < 100000m then 11500m else if income > 123000m then 0m else raise (new NotImplementedException())

    let IncomeTax income = 
        let threshold = incomeTaxAllowance income
        let Allowance income = 
            let band = threshold
            let taxed = if income < band then income else band 
            (income - taxed, 0m) // remaining to tax, tax amount
    
        let BasicIncomeTax income = 
            let band = 45000m - threshold
            let taxed = if income < band then income else band 
            (income - taxed, taxed * 0.2m) // remaining to tax, tax amount

        let HigherIncomeTax income = 
            let band = 150000m - 45000m
            let taxed = if income < band then income else band 
            (income - taxed, taxed * 0.4m) // remaining to tax, tax amount
    
        let AdditionalIncomeTax income = 
            let taxed = income
            (income - taxed, taxed * 0.5m) // remaining to tax, tax amount

        let (remain1, tax1) = Allowance income  
        let (remain2, tax2) = BasicIncomeTax remain1
        let (remain3, tax3) = HigherIncomeTax remain2
        let (remain4, tax4) = AdditionalIncomeTax remain3
        [
            ("Income Tax Basic", tax2)
            ("Income Tax Higher", tax3)
            ("Income Tax Additional", tax4)
        ]

    let EmployeeNI income =
        let threshold = 52m * 157m
        let Allowance income = 
            let band = threshold
            let taxed = if income < band then income else band 
            (income - taxed, 0m) // remaining to tax, tax amount
        let Basic income = 
            let band = 45000m - threshold
            let taxed = if income < band then income else band 
            (income - taxed, taxed * 0.12m) // remaining to tax, tax amount
        let Higher income = 
            let taxed = income
            (income - taxed, taxed * 0.02m) // remaining to tax, tax amount
        let (remain1, tax1) = Allowance income  
        let (remain2, tax2) = Basic remain1
        let (remain3, tax3) = Higher remain2
        [
            ("Employee NI Basic", tax2)
            ("Employee NI Higher", tax3)
        ]

    let EmployerNI income =
        let threshold = 52m * 157m
        let Allowance income = 
            let band = threshold
            let taxed = if income < band then income else band 
            (income - taxed, 0m) // remaining to tax, tax amount
        let Basic income = 
            let band = 45000m - threshold
            let taxed = if income < band then income else band 
            (income - taxed, taxed * 0.138m) // remaining to tax, tax amount
        let Higher income = 
            let taxed = income
            (income - taxed, taxed * 0.138m) // remaining to tax, tax amount
        let (remain1, tax1) = Allowance income  
        let (remain2, tax2) = Basic remain1
        let (remain3, tax3) = Higher remain2
        [
            ("Company NI Basic", tax2)
            ("Company NI Higher", tax3)
        ]

    let ReportTax lst =
            for pair in lst do
            match pair with
            | (band, tax) -> if tax > 0m then Console.WriteLine("{0} {1:N0}", band, tax)

    let Employee income =
        let salary = income * 0.87m // allow for employer NI
        let incometax = IncomeTax salary
        let eeNI = EmployeeNI salary
        let erNI = EmployerNI salary
        ReportTax incometax
        ReportTax eeNI
        ReportTax erNI

        let employerNI = List.fold (fun acc (_, tax) -> acc + tax ) 0m erNI
        Console.WriteLine("Salary {0:N0}, EmployerNI {1:N0}, Total {2:N0}, Income {3:N0}", salary, employerNI, salary + employerNI, income)
        let tax = List.fold (fun acc (_, tax) -> acc + tax ) 0m incometax + List.fold (fun acc (_, tax) -> acc + tax ) 0m eeNI
        Console.WriteLine("Tax {0:N0}, Salary after tax {1:N0}", tax, salary - tax)

    let DividendTax income profit =
        let remainingAllowance = incomeTaxAllowance income
        let divsAllowance = 2000m + if income < remainingAllowance then remainingAllowance - income else 0m
        let Allowance profit = 
            let band = divsAllowance
            let taxed = if profit < band then profit else band 
            (profit - taxed, 0m) // remaining to tax, tax amount
        let Basic profit = 
            let band = 45000m - divsAllowance
            let taxed = if profit < band then profit else band 
            (profit - taxed, taxed * 0.075m) // remaining to tax, tax amount
        let Higher profit = 
            let band = 150000m - 45000m
            let taxed = if profit < band then profit else band 
            (profit - taxed, taxed * 0.325m) // remaining to tax, tax amount
        let Additional profit = 
            let taxed = profit
            (profit - taxed, taxed * 0.381m) // remaining to tax, tax amount

        let (remain1, tax1) = Allowance profit
        let (remain2, tax2) = Basic remain1
        let (remain3, tax3) = Higher remain2
        let (remain4, tax4) = Additional remain3
        [
            ("Divs Basic", tax2)
            ("Divs Higher", tax3)
            ("Divs Additional", tax4)
        ]


    let Director income = 
        let Shareholder salary grossDivs = 
            let divsTax = DividendTax salary grossDivs
            ReportTax divsTax        
            let tax = List.fold (fun acc (_, tax) -> acc + tax ) 0m divsTax
            Console.WriteLine("Gross Divs {0:N0}, Tax {1:N0}, Dividends after tax {2:N0}", grossDivs, tax, grossDivs - tax)

        let salary = 12000m//157m * 52m
        let incometax = IncomeTax salary
        let eeNI = EmployeeNI salary
        let erNI = EmployerNI salary

        ReportTax incometax
        ReportTax eeNI
        ReportTax erNI

        let employerNI = List.fold (fun acc (_, tax) -> acc + tax ) 0m erNI
        Console.WriteLine("Salary {0:N0}, EmployerNI {1:N0}, Total {2:N0}, Income {3:N0}", salary, employerNI, salary + employerNI, income)
        let tax = List.fold (fun acc (_, tax) -> acc + tax ) 0m incometax + List.fold (fun acc (_, tax) -> acc + tax ) 0m eeNI
        Console.WriteLine("Tax {0:N0}, Salary after tax {1:N0}", tax, salary - tax)

        let expenses = 110m * 12m
        let grossProfit = income - salary - employerNI - expenses
        let corpTax = CorporationTax grossProfit
        let netProfit = grossProfit - corpTax
        Console.WriteLine("Gross profile {0:n0}, Corpation Tax {1:N0}, Net profit {2:N0}", grossProfit, corpTax, netProfit)

        Console.WriteLine("--- Main share holder ---");
        Shareholder salary (netProfit * 0.51m)
        Console.WriteLine("--- second share holder ---");
        Shareholder (200m*12m) (netProfit * 0.49m)


    [<EntryPoint>]
    let Main(args) = 
        Employee income
        Console.WriteLine()
        Director income
        0