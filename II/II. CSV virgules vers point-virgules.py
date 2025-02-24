import pandas as pd


file_path = "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Base pop/dép-rég.csv"
data = pd.read_csv(file_path, sep=',', encoding='utf-8')

data.columns = [col.strip().replace('\ufeff', '') for col in data.columns]

# Sauvegarder un nouveau fichier propre
output_path = "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Base pop/Dép&Rég - Généré avec Claude.csv"
data.to_csv(output_path, index=False, sep=';', encoding='utf-8')

print("Fait")