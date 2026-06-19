# el pipeline DiseñoINE produce una muestra estable (semilla = 123)

    Code
      as.data.frame(resumen_muestra)
    Output
          region MUNICIPIO SECCION manzanas
      1 Region 1   MUN_1_1    1102        4
      2 Region 1   MUN_1_1    1103        4
      3 Region 1   MUN_1_2    1104        4
      4 Region 1   MUN_1_2    1105        4
      5 Region 2   MUN_2_1    1201        4
      6 Region 2   MUN_2_1    1203        4
      7 Region 2   MUN_2_2    1205        4
      8 Region 2   MUN_2_2    1206        4

# las cuotas del pipeline son estables (semilla = 123)

    Code
      as.data.frame(cuotas)
    Output
                 Municipio cluster_2  rango sexo n
      1  Municipio MUN_1_1         2  18A24    F 4
      2  Municipio MUN_1_1         2  18A24    M 4
      3  Municipio MUN_1_1         2  25A39    F 5
      4  Municipio MUN_1_1         2  25A39    M 5
      5  Municipio MUN_1_1         2  40A59    F 5
      6  Municipio MUN_1_1         2  40A59    M 4
      7  Municipio MUN_1_1         2 60YMAS    F 2
      8  Municipio MUN_1_1         2 60YMAS    M 2
      9  Municipio MUN_1_1         3  18A24    F 3
      10 Municipio MUN_1_1         3  18A24    M 3
      11 Municipio MUN_1_1         3  25A39    F 4
      12 Municipio MUN_1_1         3  25A39    M 4
      13 Municipio MUN_1_1         3  40A59    F 4
      14 Municipio MUN_1_1         3  40A59    M 4
      15 Municipio MUN_1_1         3 60YMAS    F 2
      16 Municipio MUN_1_1         3 60YMAS    M 2
      17 Municipio MUN_1_2         4  18A24    F 5
      18 Municipio MUN_1_2         4  18A24    M 4
      19 Municipio MUN_1_2         4  25A39    F 6
      20 Municipio MUN_1_2         4  25A39    M 5
      21 Municipio MUN_1_2         4  40A59    F 5
      22 Municipio MUN_1_2         4  40A59    M 5
      23 Municipio MUN_1_2         4 60YMAS    F 3
      24 Municipio MUN_1_2         4 60YMAS    M 2
      25 Municipio MUN_1_2         5  18A24    F 4
      26 Municipio MUN_1_2         5  18A24    M 4
      27 Municipio MUN_1_2         5  25A39    F 5
      28 Municipio MUN_1_2         5  25A39    M 5
      29 Municipio MUN_1_2         5  40A59    F 5
      30 Municipio MUN_1_2         5  40A59    M 4
      31 Municipio MUN_1_2         5 60YMAS    F 2
      32 Municipio MUN_1_2         5 60YMAS    M 2
      33 Municipio MUN_2_1         7  18A24    F 4
      34 Municipio MUN_2_1         7  18A24    M 4
      35 Municipio MUN_2_1         7  25A39    F 5
      36 Municipio MUN_2_1         7  25A39    M 4
      37 Municipio MUN_2_1         7  40A59    F 4
      38 Municipio MUN_2_1         7  40A59    M 4
      39 Municipio MUN_2_1         7 60YMAS    F 2
      40 Municipio MUN_2_1         7 60YMAS    M 3
      41 Municipio MUN_2_1         9  18A24    F 5
      42 Municipio MUN_2_1         9  18A24    M 4
      43 Municipio MUN_2_1         9  25A39    F 6
      44 Municipio MUN_2_1         9  25A39    M 5
      45 Municipio MUN_2_1         9  40A59    F 5
      46 Municipio MUN_2_1         9  40A59    M 5
      47 Municipio MUN_2_1         9 60YMAS    F 3
      48 Municipio MUN_2_1         9 60YMAS    M 2
      49 Municipio MUN_2_2        11  18A24    F 3
      50 Municipio MUN_2_2        11  18A24    M 3
      51 Municipio MUN_2_2        11  25A39    F 4
      52 Municipio MUN_2_2        11  25A39    M 4
      53 Municipio MUN_2_2        11  40A59    F 4
      54 Municipio MUN_2_2        11  40A59    M 4
      55 Municipio MUN_2_2        11 60YMAS    F 2
      56 Municipio MUN_2_2        11 60YMAS    M 2
      57 Municipio MUN_2_2        12  18A24    F 3
      58 Municipio MUN_2_2        12  18A24    M 3
      59 Municipio MUN_2_2        12  25A39    F 4
      60 Municipio MUN_2_2        12  25A39    M 4
      61 Municipio MUN_2_2        12  40A59    F 4
      62 Municipio MUN_2_2        12  40A59    M 4
      63 Municipio MUN_2_2        12 60YMAS    F 2
      64 Municipio MUN_2_2        12 60YMAS    M 2

