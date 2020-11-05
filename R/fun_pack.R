fun_pack <- function(dane = input,
                     kategoria = "Passenger Cars",
                     paliwo = "Petrol",
                     segment = "Mini",
                     euro = "Euro 5",
                     technologia = "GDI",
                     mode = "",
                     substancja = c("EC", "CO")) {



  out <- wskazniki %>%
    filter(Category %in% kategoria) %>%
    filter(Fuel %in% paliwo) %>%
    filter(Euro.Standard %in% euro) %>%
    filter(Technology %in% technologia) %>%
    filter(Pollutant %in% substancja) %>%
    filter(Mode %in% mode)

  out <- inner_join(x = out, y = input, by = "Segment","Fuel")

  out <- out %>%
    mutate(Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction))
    ) %>%
    select(Category, Fuel, Euro.Standard, Technology, Pollutant, Mode, Segment, Nat, Emisja)

  return(out)

}


