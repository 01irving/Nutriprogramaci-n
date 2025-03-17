import streamlit as st

def calcular_ree(edad, peso_pregestacional, talla, naf, lactancia=False, semestre=None, bien_alimentada=False):
    """
    Calcula el Requerimiento Energético Estimado (REE) para mujeres adolescentes y adultas.
    Incluye el costo energético para la producción de leche durante la lactancia.
    Resta 170 kcal/día si estuvo bien alimentada durante el embarazo y está en el primer semestre de lactancia.
    
    Parámetros:
        edad (int): Edad en años
        peso_pregestacional (float): Peso antes del embarazo en kg
        talla (float): Talla en metros
        naf (float): Nivel de Actividad Física
        lactancia (bool): Indica si la mujer está lactando
        semestre (int): Semestre de lactancia (1 o 2)
        bien_alimentada (bool): Indica si estuvo bien alimentada durante el embarazo
    
    Retorna:
        float: REE calculado en kcal/día
    """
    deposito_energia = {12: 26, 13: 24, 14: 19, 15: 12, 16: 5, 17: 0}
    
    if 12 <= edad < 18:
        ree = (65.3 * peso_pregestacional) - (0.454 * (peso_pregestacional ** 2)) + 263.4 + deposito_energia.get(edad, 0)
        ger = None  # GER no se calcula para este rango de edad
    elif 18 <= edad < 30:
        ger = (14.818 * peso_pregestacional) + 486.6
        ree = ger * naf
    elif edad >= 30:
        ger = (8.126 * peso_pregestacional) + 845.6
        ree = ger * naf
    else:
        raise ValueError("Edad fuera del rango permitido (12-50 años)")
    
    # Añadir costo energético por lactancia si corresponde
    if lactancia:
        if semestre == 1:
            ree += 675  # Costo energético para el primer semestre de lactancia
            # Restar 170 kcal/día si estuvo bien alimentada durante el embarazo
            if bien_alimentada:
                ree -= 170
        elif semestre == 2:
            ree += 460  # Costo energético para el segundo semestre de lactancia
        else:
            raise ValueError("Semestre de lactancia no válido. Debe ser 1 o 2.")
    
    return ree, ger

# Configuración de la app de Streamlit
st.title("Cálculo de Requerimiento Energético Estimado (REE) durante la lactancia")
st.markdown("""
Esta aplicación calcula el **Requerimiento Energético Estimado (REE)** para mujeres adolescentes y adultas.
Incluye el costo energético para la producción de leche durante la lactancia y ajustes por alimentación durante el embarazo.
**Nota:** Se utiliza el peso pregestacional (peso antes del embarazo) para el cálculo.
""")

# Entradas del usuario
edad = st.number_input("Ingrese la edad en años:", min_value=12, max_value=50, value=25)
peso_pregestacional = st.number_input("Ingrese el peso pregestacional en kg:", min_value=30.0, max_value=200.0, value=60.0)
talla = st.number_input("Ingrese la talla en metros:", min_value=1.0, max_value=2.5, value=1.65)
naf = st.number_input(
    "Ingrese el nivel de actividad física (NAF):",
    min_value=1.40, max_value=2.40, value=1.60, step=0.01,
    help="Sedentario 1.40-1.69, Activo 1.70-1.99, Vigoroso 2.00-2.40"
)
st.markdown("""
- **Sedentario:** 1.40-1.69
- **Activo:** 1.70-1.99
- **Vigoroso:** 2.00-2.40
""")

# Opciones para lactancia
lactancia = st.checkbox("¿Está lactando?")
semestre = None
bien_alimentada = False
if lactancia:
    semestre = st.radio(
        "Seleccione el semestre de lactancia:",
        options=[1, 2],
        index=0,
        help="1: Primer semestre (675 kcal/día), 2: Segundo semestre (460 kcal/día)"
    )
    if semestre == 1:
        bien_alimentada = st.checkbox(
            "¿Estuvo bien alimentada durante el embarazo?",
            help="Si estuvo bien alimentada, se restarán 170 kcal/día en el primer semestre de lactancia."
        )

# Botón para calcular
if st.button("Calcular REE"):
    try:
        ree, ger = calcular_ree(edad, peso_pregestacional, talla, naf, lactancia, semestre, bien_alimentada)
        st.success(f"El Requerimiento Energético Estimado (REE) es: **{ree:.2f} kcal/día**")
        if ger is not None:
            st.info(f"El Gasto Energético en Reposo (GER) es: **{ger:.2f} kcal/día**")
    except ValueError as e:
        st.error(e)
        