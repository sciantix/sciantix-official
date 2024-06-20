#!/usr/bin/env python
# coding: utf-8

# In[1]:


import argparse
import tkinter as tk
from tkinter import scrolledtext, ttk

def lire_et_afficher_tableau(filename):
    try:
        with open(filename, 'r') as file:
            # Lire toutes les lignes du fichier
            lignes = file.readlines()

            # Vérifier si le fichier est vide
            if not lignes:
                print(f"Le fichier '{filename}' est vide.")
                return

            # Fonction pour extraire les valeurs de chaque ligne en utilisant des tabulations comme séparateurs
            def extraire_valeurs(ligne):
                valeurs = ligne.strip().split('\t')
                return valeurs

            # Titres de colonnes (première ligne du fichier)
            first_line = lignes[0].strip()
            titres_colonnes = first_line.split('\t')

            # Calculer la largeur maximale pour chaque colonne
            largeur_colonnes = [len(titre) for titre in titres_colonnes]
            for ligne in lignes[1:]:
                valeurs = extraire_valeurs(ligne)
                for i, valeur in enumerate(valeurs):
                    largeur_colonnes[i] = max(largeur_colonnes[i], len(valeur))

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

                # Insérer les valeurs pour chaque ligne à partir de la deuxième ligne avec séparation entre chaque colonne
                for ligne in lignes[1:]:
                    valeurs = extraire_valeurs(ligne)
                    formatted_line = ''
                    for i, valeur in enumerate(valeurs):
                        formatted_line += f"{valeur.center(largeur_colonnes[i])}"
                        if i < len(valeurs) - 1:
                            formatted_line += ' | '
                    text_area.insert(tk.END, formatted_line + '\n')
                
                    # Insérer une ligne de séparation après chaque ligne de valeurs
                    text_area.insert(tk.END, '-' * (sum(largeur_colonnes) + (len(valeurs) - 1) * 3) + '\n')

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


# In[ ]:




