import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import sawtooth

import sympy as sp
def raiz_soma_quadrados(*numeros):
    return np.linalg.norm(numeros)
print(raiz_soma_quadrados(3, 4))

# Definir variáveis
t, T, f0, m = sp.symbols('t T f0 m', real=True)
f = 1 + 4*f0*t  # Função para t em [-T/2, 0]
g = 1 - 4*f0*t  # Função para t em [0, T/2]
omega = 2 * sp.pi * (2*m - 1) * f0

# Calcular as integrais
integral_1 = sp.integrate(f * sp.cos(omega * t), (t, -T/2, 0))
integral_2 = sp.integrate(g * sp.cos(omega * t), (t, 0, T/2))

# Expressão final de Am
A_m = (2/T) * (integral_1 + integral_2)

# Simplificar o resultado
A_m = sp.simplify(A_m)
print(A_m)

"""
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 1e-6  # 1 µF
f0 = 1e3  # Frequência fundamental (1 kHz)
T = 1 / f0  # Período da onda
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(-T/2, T/2, 1000)

# Série de Fourier da função v0(t)
m_values = np.arange(1, 11)  # 10 primeiros termos
A_m = (8 / (np.pi**2 * (2*m_values - 1)**2))  # Coeficientes
v0_fourier = np.zeros_like(t)

for m in m_values:
    v0_fourier += A_m[m-1] * np.cos(2 * np.pi * (2*m - 1) * f0 * t)

# Resposta do sistema para cada harmônico
v1_harmonics = {}
for m in m_values:
    f_m = (2*m - 1) * f0  # Frequência do harmônico
    omega_m = 2 * np.pi * f_m
    H_m = 1 / np.sqrt(1 + (omega_m * tau)**2)  # Módulo da resposta
    phi_m = -np.arctan(omega_m * tau)  # Fase da resposta
    v1_harmonics[m] = H_m * A_m[m-1] * np.cos(omega_m * t + phi_m)

# Plotando os sinais
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v0_fourier, label="$v_0(t)$ (Série de Fourier Aproximada)", linestyle='dashed')
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Função $v_0(t)$ e sua Série de Fourier")
plt.legend()
plt.grid()
plt.show()

# Plotando a resposta para os harmônicos
plt.figure(figsize=(8, 4))
for m in m_values:
    plt.plot(t * 1e3, v1_harmonics[m], label=f"Harmônico {m}")

plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Resposta do Circuito RC para os Harmônicos")
plt.legend()
plt.grid()
plt.show()
"""

"""
#4 e 5 senoide?
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 100e-9  # 1 µF
f0 = 1e3  # Frequência fundamental (1 kHz)
T = 1 / f0  # Período da onda
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(0, 2*T, 1000)  # Dois períodos

# Fonte senoidal
v0 = 1 * np.sin(2 * np.pi * f0 * t)  # 2 Vpp → ±1V

# Cálculo da resposta do sistema para harmônicos
m_values = np.arange(1, 11)  # 10 primeiros harmônicos
v1_approx = np.zeros_like(t)

for m in m_values:
    f_m = (2*m - 1) * f0  # Frequência do harmônico
    omega_m = 2 * np.pi * f_m
    H_m = 1 / np.sqrt(1 + (omega_m * tau)**2)  # Módulo da resposta
    phi_m = -np.arctan(omega_m * tau)  # Fase da resposta
    v1_approx += H_m * np.sin(omega_m * t + phi_m)

# Plotando os sinais
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v0, label="$v_0(t)$ (Fonte senoidal)", linestyle='dashed')
plt.plot(t * 1e3, v1_approx, label="$\hat{v}_1(t)$ (Aproximação com 10 termos)", linewidth=2)
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Resposta do Circuito RC para Fonte Senoidal")
plt.legend()
plt.grid()
plt.show()

"""
"""
#6
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 1e-6  # 1 µF
T = 1e-3  # Período da onda (1 kHz)
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(-T/2, T/2, 1000)  # Um período centrado em t=0

# Definição da função v0(t) com base na equação fornecida
v0 = np.piecewise(t, [t <= 0, t > 0], [lambda t: 1 + 4 * (1e3) * t, lambda t: 1 - 4 * (1e3) * t])

# Cálculo da série de Fourier truncada
m_values = np.arange(1, 11)  # 10 primeiros harmônicos
A_m = (8 / (np.pi**2 * (2*m_values - 1)**2))  # Coeficientes da série

# Resposta do sistema para cada harmônico
v1_approx = np.zeros_like(t)
for m in m_values:
    f_m = (2*m - 1) * 1e3  # Frequência em Hz
    omega_m = 2 * np.pi * f_m
    H_m = 1 / np.sqrt(1 + (omega_m * tau)**2)  # Módulo da resposta do circuito RC
    phi_m = -np.arctan(omega_m * tau)  # Fase da resposta do circuito RC
    v1_approx += A_m[m-1] * H_m * np.cos(omega_m * t + phi_m)

# Plotando a aproximação v̂1(t)
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v1_approx, label="$\hat{v}_1(t)$ (Aproximação com 10 termos)")
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Aproximação da Resposta $v_1(t)$")
plt.legend()
plt.grid()
plt.show()

 """
"""
#5
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 100e-9  # 1 µF
T = 1e-3  # Período da onda (1 kHz)
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(-T/2, T/2, 1000)  # Um período centrado em t=0

# Definição da função v0(t) com base na equação fornecida
v0 = np.piecewise(t, [t <= 0, t > 0], [lambda t: 1 + 4 * (1e3) * t, lambda t: 1 - 4 * (1e3) * t])

# Cálculo da série de Fourier
m_values = np.array([1, 3, 5, 7, 9, 11, 13, 15, 17, 19])  # Harmônicos especificados em kHz
A_m = (8 / (np.pi**2 * (2*m_values - 1)**2))  # Coeficientes da série

# Resposta do sistema para cada harmônico
v1_harmonics = {}
for m in m_values:
    f_m = (2*m - 1) * 1e3  # Frequência em Hz
    omega_m = 2 * np.pi * f_m
    H_m = 1 / np.sqrt(1 + (omega_m * tau)**2)  # Resposta do circuito RC
    v1_harmonics[m] = H_m * A_m[np.where(m_values == m)[0][0]] * np.cos(omega_m * t)

# Plotando as respostas dos harmônicos
plt.figure(figsize=(8, 4))
for m in m_values:
    plt.plot(t * 1e3, v1_harmonics[m], label=f"Harmônico {m} kHz")

plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Resposta do Circuito RC para os Harmônicos")
plt.legend()
plt.grid()
plt.show()

"""
"""
#4?
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 100e-9  # 1 µF
T = 1e-3  # Período da onda (1 kHz)
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(-T/2, T/2, 1000)  # Um período centrado em t=0

# Definição da função v0(t) com base na equação fornecida
v0 = np.piecewise(t, [t <= 0, t > 0], [lambda t: 1 + 4 * (1e3) * t, lambda t: 1 - 4 * (1e3) * t])

# Cálculo da série de Fourier
m_values = np.arange(1, 10)  # Termos da série de Fourier
A_m = (8 / (np.pi**2 * (2*m_values - 1)**2))  # Coeficientes da série

# Reconstrução da série de Fourier
v0_fourier = np.zeros_like(t)
for m in m_values:
    v0_fourier += A_m[m-1] * np.cos(2 * np.pi * (2*m - 1) * 1e3 * t)

# Plotando os sinais
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v0, label="$v_0(t)$ (Função Original)", linestyle='dashed')
plt.plot(t * 1e3, v0_fourier, label="$v_0(t)$ (Série de Fourier Aproximada)", linewidth=2)
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Função $v_0(t)$ e sua Série de Fourier")
plt.legend()
plt.grid()
plt.show()


#onda trinangular com alguma onda comentada descubra



"""

"""
# Parâmetros do circuito
R = 1e3  # 1 kΩ
C = 100e-9  # 100 nF
f = 1e3  # 1 kHz
w = 2 * np.pi * f  # Frequência angular
tau = R * C  # Constante de tempo RC

# Tempo de simulação
t = np.linspace(0, 2 / f, 1000)  # Simulação de dois períodos

# Entrada senoidal
A = 1  # Amplitude de 1V
v0 = A * np.cos(w * t)  # v0(t) = A cos(wt)

# Função de transferência do circuito RC
H_w = 1 / (1 + 1j * w * R * C)  # H(jw) = 1 / (1 + jwRC)

# Resposta fasorial
V1 = H_w * A * np.exp(1j * w * t)  # V1(jw)
v1 = np.real(V1)  # Tomamos a parte real para obter no tempo

# Plotando os sinais
plt.figure(figsize=(8, 4))
plt.plot(t * 1e3, v0, label="$v_0(t)$ (Entrada Senoidal)", linestyle='dashed')
plt.plot(t * 1e3, v1, label="$v_1(t)$ (Resposta RC)", linewidth=2)
plt.xlabel("Tempo (ms)")
plt.ylabel("Tensão (V)")
plt.title("Resposta do Circuito RC a uma Onda Senoidal")
plt.legend()
plt.grid()
plt.show()


"""
while False:
    print("1. Calcular erro")
    print("2. Calcular forrier")
    escolha = input("Qual vc quer? ")
    if escolha == "1":
        valorcalculado = float(input("Valor calculado: "))
        valormedido = float(input("Valor medido: "))
        erro = ( (valorcalculado - valormedido) / valorcalculado) * 100
        erro = round(erro, 5)
        print(f"O erro é de : {erro}")
    elif escolha == "2":
        print("ainda não!")