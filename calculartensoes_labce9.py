import numpy as np
import math
import sympy as sp #qq coisa pip install sympy
import matplotlib.pyplot as plt
from scipy.signal import sawtooth

def arctan_em_graus(x):
    # Calcula o arctangente de x (em radianos)
    radianos = math.atan(x)
    # Converte de radianos para graus
    graus = math.degrees(radianos)
    return graus

def raiz_soma_quadrados(*numeros):
    return math.sqrt(sum(x**2 for x in numeros))

# Definição das variáveis simbólicas
frequencias = [1000,3000,5000,7000,9000,11000,13000,15000,17000,19000]
for frequencia in frequencias:
    print(f"\nfrequencia de :{frequencia}\n")
    V0, R, C, w = sp.symbols('V0 R C w')  # Tensão de entrada, resistência, capacitância, frequência angular
    w = frequencia * 2 * np.pi
    C = 100e-9
    R = 1000
    V0 = 1

    # Impedância do capacitor
    Z_C = 1 / (1j * w * C)
    print(f"impedancia do capacitor:{Z_C}")

    # Impedância total do circuito
    Z_total = R + Z_C
    z = raiz_soma_quadrados(sp.im(Z_C), 1000)
    fasez = sp.im(Z_C)/1000
    faseZ = arctan_em_graus(fasez)

    # Expressão da corrente
    I = V0 / Z_total
    I_real = sp.re(I)
    I_imag = sp.im(I)

    # Calcular o módulo de V1
    fasei = I_imag/I_real
    faseI = arctan_em_graus(fasei)
    I_modulo = raiz_soma_quadrados(I_real, I_imag)
    print(f"corrente total do circuito: {I}")
    print(f"modulo da corrente e fase :{I_modulo} , {faseI}")

    # Tensão sobre o capacitor (V1)
    V1 = I * Z_C

    # Exibição do resultado
    print("Expressão de V1:")
    sp.pprint(V1)
    V1_real = sp.re(V1)
    V1_imag = sp.im(V1)

    # Calcular o módulo de V1
    V1_modulo = raiz_soma_quadrados(V1_real, V1_imag)
    faseV = V1_imag/V1_real
    faseV1 = arctan_em_graus(faseV)
    diferençafase = faseV1
    print(f"Módulo de V1 : {V1_modulo}. e fase : {diferençafase}")

# Definição de parâmetros
f0 = 1e3  # Frequência fundamental (1 kHz)
T = 1 / f0  # Período da onda
t = np.linspace(-T/2, T/2, 1000)  # Intervalo de tempo

# Cálculo da série de Fourier aproximada
m_values = np.arange(1, 11)  # 10 primeiros termos
v0_fourier = np.zeros_like(t)

for m in m_values:
    Am = (8 / (np.pi**2 * (2*m - 1)**2))  # Coeficiente Am
    v0_fourier += Am * np.cos(2 * np.pi * (2*m - 1) * f0 * t)

# Plotando o gráfico
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v0_fourier, label="Aproximação da Série de Fourier", color='b')
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Aproximação de v₀(t) com Série de Fourier (10 termos)")
plt.legend()
plt.grid()
plt.show()
