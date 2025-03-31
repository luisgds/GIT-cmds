import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import sawtooth
import math
import sympy as sp

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
