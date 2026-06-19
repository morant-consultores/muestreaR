# el pipeline DiseñoINE produce una muestra estable (semilla = 123)

    Code
      as.data.frame(resumen_muestra)
    Output
          region MUNICIPIO SECCION manzanas
      1 Region 1   MUN_A_1    1000        3
      2 Region 1   MUN_A_1    1002        4
      3 Region 1   MUN_A_2    1003        3
      4 Region 1   MUN_A_2    1004        4
      5 Region 2   MUN_B_1    1006        3
      6 Region 2   MUN_B_1    1009        1
      7 Region 2   MUN_B_2    1010        4
      8 Region 2   MUN_B_2    1011        4

# las cuotas del pipeline son estables (semilla = 123)

    Code
      as.data.frame(cuotas)
    Output
                 Municipio cluster_3  rango sexo n
      1  Municipio MUN_A_1         1  18A24    F 4
      2  Municipio MUN_A_1         1  18A24    M 4
      3  Municipio MUN_A_1         1  25A39    F 5
      4  Municipio MUN_A_1         1  25A39    M 5
      5  Municipio MUN_A_1         1  40A59    F 5
      6  Municipio MUN_A_1         1  40A59    M 4
      7  Municipio MUN_A_1         1 60YMAS    F 3
      8  Municipio MUN_A_1         1 60YMAS    M 1
      9  Municipio MUN_A_1         3  18A24    F 4
      10 Municipio MUN_A_1         3  18A24    M 3
      11 Municipio MUN_A_1         3  25A39    F 5
      12 Municipio MUN_A_1         3  25A39    M 4
      13 Municipio MUN_A_1         3  40A59    F 4
      14 Municipio MUN_A_1         3  40A59    M 5
      15 Municipio MUN_A_1         3 60YMAS    F 2
      16 Municipio MUN_A_1         3 60YMAS    M 2
      17 Municipio MUN_A_2         5  18A24    F 4
      18 Municipio MUN_A_2         5  18A24    M 3
      19 Municipio MUN_A_2         5  25A39    F 6
      20 Municipio MUN_A_2         5  25A39    M 4
      21 Municipio MUN_A_2         5  40A59    F 4
      22 Municipio MUN_A_2         5  40A59    M 4
      23 Municipio MUN_A_2         5 60YMAS    F 2
      24 Municipio MUN_A_2         5 60YMAS    M 2
      25 Municipio MUN_A_2         6  18A24    F 5
      26 Municipio MUN_A_2         6  18A24    M 5
      27 Municipio MUN_A_2         6  25A39    F 6
      28 Municipio MUN_A_2         6  25A39    M 5
      29 Municipio MUN_A_2         6  40A59    F 6
      30 Municipio MUN_A_2         6  40A59    M 5
      31 Municipio MUN_A_2         6 60YMAS    F 3
      32 Municipio MUN_A_2         6 60YMAS    M 3
      33 Municipio MUN_B_1         9  18A24    F 3
      34 Municipio MUN_B_1         9  18A24    M 3
      35 Municipio MUN_B_1         9  25A39    F 4
      36 Municipio MUN_B_1         9  25A39    M 4
      37 Municipio MUN_B_1         9  40A59    F 4
      38 Municipio MUN_B_1         9  40A59    M 4
      39 Municipio MUN_B_1         9 60YMAS    F 2
      40 Municipio MUN_B_1         9 60YMAS    M 2
      41 Municipio MUN_B_1        12  18A24    F 1
      42 Municipio MUN_B_1        12  18A24    M 1
      43 Municipio MUN_B_1        12  25A39    F 1
      44 Municipio MUN_B_1        12  25A39    M 1
      45 Municipio MUN_B_1        12  40A59    F 1
      46 Municipio MUN_B_1        12  40A59    M 1
      47 Municipio MUN_B_1        12 60YMAS    F 0
      48 Municipio MUN_B_1        12 60YMAS    M 0
      49 Municipio MUN_B_2        14  18A24    F 5
      50 Municipio MUN_B_2        14  18A24    M 5
      51 Municipio MUN_B_2        14  25A39    F 6
      52 Municipio MUN_B_2        14  25A39    M 6
      53 Municipio MUN_B_2        14  40A59    F 6
      54 Municipio MUN_B_2        14  40A59    M 6
      55 Municipio MUN_B_2        14 60YMAS    F 3
      56 Municipio MUN_B_2        14 60YMAS    M 3
      57 Municipio MUN_B_2        15  18A24    F 5
      58 Municipio MUN_B_2        15  18A24    M 5
      59 Municipio MUN_B_2        15  25A39    F 7
      60 Municipio MUN_B_2        15  25A39    M 6
      61 Municipio MUN_B_2        15  40A59    F 6
      62 Municipio MUN_B_2        15  40A59    M 6
      63 Municipio MUN_B_2        15 60YMAS    F 3
      64 Municipio MUN_B_2        15 60YMAS    M 3

