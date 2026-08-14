#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import argparse
import tkinter as tk
from tkinter import scrolledtext, ttk
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

def lire_et_afficher_tableau(filename):
    try:
        # Lire le fichier en utilisant pandas sans utiliser la première colonne comme index
        data = pd.read_csv(filename, sep='\t', dtype=str, index_col=False)  # Lire toutes les données comme des chaînes de caractères
        data = data.fillna('')  # Remplacer les valeurs NaN par des chaînes vides

        # Vérifier si le fichier est vide
        if data.empty:
            print(f"Le fichier '{filename}' est vide.")
            return

        # Titres de colonnes
        titres_colonnes = data.columns.tolist()

        # Calculer la largeur maximale pour chaque colonne
        largeur_colonnes = [len(titre) for titre in titres_colonnes]
        for index, row in data.iterrows():
            for i, valeur in enumerate(row):
                largeur_colonnes[i] = max(largeur_colonnes[i], len(valeur))

        # Élargir les colonnes
        largeur_colonnes = [largeur + 5 for largeur in largeur_colonnes]  # Ajouter 5 pour élargir les colonnes

        # Initialiser Tkinter
        root = tk.Tk()
        root.title(f"Contenu du fichier : {filename}")

        # Mettre la fenêtre en plein écran
        root.attributes('-fullscreen', True)

        # Définir les options de police disponibles
        polices_disponibles = ['Courier New', 'Arial', 'Times New Roman']

        # Variables pour stocker la sélection de l'utilisateur
        selected_police = tk.StringVar(value='Courier New')
        selected_taille = tk.IntVar(value=10)

        # Fonction pour mettre à jour la police du ScrolledText
        def update_font():
            nouvelle_police = selected_police.get()
            nouvelle_taille = selected_taille.get()
            text_area.configure(font=(nouvelle_police, nouvelle_taille))
            update_display()

        # Fonction pour mettre à jour l'affichage avec l'alignement des colonnes et les séparations entre colonnes
        def update_display():
            text_area.configure(state='normal')
            text_area.delete('1.0', tk.END)

            # Insérer les titres des colonnes avec séparation entre chaque colonne
            formatted_line = ''
            for i, titre in enumerate(titres_colonnes):
                formatted_line += f"{titre.center(largeur_colonnes[i])}"
                if i < len(titres_colonnes) - 1:
                    formatted_line += ' | '
            text_area.insert(tk.END, formatted_line + '\n')

            # Insérer une ligne de séparation sous les titres
            text_area.insert(tk.END, '-' * (sum(largeur_colonnes) + (len(titres_colonnes) - 1) * 3) + '\n')

            # Insérer les valeurs pour chaque ligne avec séparation entre chaque colonne
            for index, row in data.iterrows():
                formatted_line = ''
                for i, valeur in enumerate(row):
                    formatted_line += f"{valeur.center(largeur_colonnes[i])}"
                    if i < len(row) - 1:
                        formatted_line += ' | '
                text_area.insert(tk.END, formatted_line + '\n')

                # Insérer une ligne de séparation après chaque ligne de valeurs
                text_area.insert(tk.END, '-' * (sum(largeur_colonnes) + (len(row) - 1) * 3) + '\n')

            text_area.configure(state='disabled')

        # Cadre pour les options de police
        cadre_options = ttk.Frame(root)
        cadre_options.pack(pady=10)

        # Menu déroulant pour sélectionner la police
        ttk.Label(cadre_options, text="Police :").grid(row=0, column=0, padx=5, pady=5)
        police_menu = ttk.Combobox(cadre_options, textvariable=selected_police, values=polices_disponibles, state='readonly', width=15)
        police_menu.grid(row=0, column=1, padx=5, pady=5)
        police_menu.bind('<<ComboboxSelected>>', lambda event: update_font())

        # Scale pour sélectionner la taille de police
        ttk.Label(cadre_options, text="Taille :").grid(row=0, column=2, padx=5, pady=5)
        taille_scale = ttk.Scale(cadre_options, from_=8, to=20, orient='horizontal', variable=selected_taille, command=lambda value: update_font())
        taille_scale.grid(row=0, column=3, padx=5, pady=5)

        # Ajouter un widget ScrolledText pour afficher le contenu avec une taille plus grande
        text_area = scrolledtext.ScrolledText(root, wrap=tk.WORD)
        text_area.pack(expand=True, fill='both', padx=10, pady=10)

        # Utiliser une police à chasse fixe avec une taille initiale
        text_area.configure(font=(selected_police.get(), selected_taille.get()), state='disabled')

        # Mettre à jour l'affichage initial
        update_display()

        # Ajouter une fonctionnalité pour quitter le plein écran avec la touche 'Escape'
        def quit_fullscreen(event):
            root.attributes('-fullscreen', False)

        root.bind('<Escape>', quit_fullscreen)

        # Cadre pour les options de graphique
        cadre_graphique = ttk.Frame(root)
        cadre_graphique.pack(pady=10)

        # Variables pour stocker la sélection des colonnes pour les axes X et Y
        selected_x = tk.StringVar(value=titres_colonnes[0])
        selected_y = tk.StringVar(value=titres_colonnes[1])

        # Fonction pour tracer le graphique
        def plot_graph():
            x_col = selected_x.get()
            y_col = selected_y.get()

            # Convertir les colonnes sélectionnées en numérique, gérer les erreurs de conversion
            try:
                x_data = pd.to_numeric(data[x_col], errors='coerce').dropna()
                y_data = pd.to_numeric(data[y_col], errors='coerce').dropna()
                plt.figure(figsize=(10, 6))
                plt.plot(x_data, y_data, marker='o')
                plt.xlabel(x_col)
                plt.ylabel(y_col)
                plt.title(f"Graphique de {y_col} en fonction de {x_col}")

                # Afficher le graphique dans une nouvelle fenêtre Tkinter
                fig = plt.gcf()
                top = tk.Toplevel()
                top.title(f"Graphique : {y_col} vs {x_col}")
                canvas = FigureCanvasTkAgg(fig, master=top)
                canvas.draw()
                canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
            except Exception as e:
                print(f"Erreur lors du tracé du graphique : {e}")

        # Menu déroulant pour sélectionner la colonne X
        ttk.Label(cadre_graphique, text="Colonne X :").grid(row=0, column=0, padx=5, pady=5)
        x_menu = ttk.Combobox(cadre_graphique, textvariable=selected_x, values=titres_colonnes, state='readonly', width=15)
        x_menu.grid(row=0, column=1, padx=5, pady=5)

        # Menu déroulant pour sélectionner la colonne Y
        ttk.Label(cadre_graphique, text="Colonne Y :").grid(row=0, column=2, padx=5, pady=5)
        y_menu = ttk.Combobox(cadre_graphique, textvariable=selected_y, values=titres_colonnes, state='readonly', width=15)
        y_menu.grid(row=0, column=3, padx=5, pady=5)

        # Bouton pour tracer le graphique
        plot_button = ttk.Button(cadre_graphique, text="Tracer le graphique", command=plot_graph)
        plot_button.grid(row=0, column=4, padx=5, pady=5)

        # Lancer la boucle principale de Tkinter
        root.mainloop()

    except FileNotFoundError:
        print(f"Le fichier '{filename}' n'a pas été trouvé.")
    except Exception as e:
        print(f"Une erreur est survenue : {e}")

# Configurer argparse pour lire le chemin du fichier à partir de la ligne de commande
parser = argparse.ArgumentParser(description="Lire et afficher un tableau de valeurs à partir d'un fichier texte.")
parser.add_argument('filename', type=str, help="Le chemin complet du fichier à lire")
args = parser.parse_args()

# Appeler la fonction pour lire et afficher le tableau
lire_et_afficher_tableau(args.filename)


# %%
