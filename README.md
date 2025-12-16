# 🐍 MiniPython

![MiniPython Logo](assets/images/logo.png)

MiniPython est un **mini-langage pédagogique** inspiré de Python, développé dans le cadre du module **Compilation 2 (4ème Ingénieur – Génie Logiciel)** à l’Université Abou Bekr Belkaid – Tlemcen.  
Il permet d’illustrer les différentes étapes de la compilation.

---

## 🎯 Objectifs

- Comprendre la conception d’un langage de programmation
- Mettre en pratique l’analyse lexicale et syntaxique
- Manipuler des grammaires formelles (BNF / LL(1))
- Documenter correctement un projet informatique
- Publier un projet académique sur GitHub avec une licence

---

## ✨ Spécification du langage MiniPython

### 🔹 Déclarations de variables
int x;
float a;
bool b;
string s;
int T[10];
float M[2][3];

🔹 Affectations
x = 3;
a = x + 2 * 5;

🔹 Expressions arithmétiques

Opérateurs : + - * /

Parenthèses ( )

🔹 Expressions booléennes

Opérateurs : && || !

🔹 Comparaisons

< > == !=

🔹 Structures de contrôle
Conditionnelle
if (x > 0) {
    print(x);
} else {
    print(0);
}

Boucle while
while (x < 10) {
    x = x + 1;
}

🔹 Affichage
print(x);

🔹 Procédures
def procedure afficherMessage(string msg) {
    print(msg);
    return;
}

🔹 Commentaires
/* Ceci est un commentaire */

⚙️ Installation et exécution
Prérequis

Python 3.8 ou plus

VS Code ou tout autre éditeur

Git (optionnel)

Installation
git clone https://github.com/votre-utilisateur/minipython.git
cd minipython

Exécution
python main.py exemple.minipython

minipython/
│── assets/
│   └── images/
│       └── logo.png
│── lexer/
│── parser/
│── examples/
│── main.py
│── README.md
│── LICENSE

pour exécuter : il faut tapez sur terminal python minipython_copiler.py

🎨 Logo MiniPython

Créé avec : Canva / Figma / Inkscape

Format : PNG ou SVG

Emplacement : assets/images/logo.png

Couleurs :

Vert : apprentissage et programmation

Jaune : pédagogie

👥 Contributeurs

Ougherb Mohammed
Matallah Abdessamed
Kara Slimane Rayane
Ichou Omar

 – 4ème ING GL – Université Abou Bekr Belkaid (Tlemcen)

📜 Licence

Ce projet est sous licence MIT.

Droits :

Utilisation libre

Modification

Redistribution

Obligations :

Mentionner l’auteur

Conserver la licence

MIT License

Copyright (c) 2025 Nom Prénom

🚀 Publication GitHub
git init
git add .
git commit -m "TP Compilation 2 – MiniPython"
git remote add origin https://github.com/votre-utilisateur/minipython.git
git push -u origin main

📌 Remarque

Ce projet est réalisé à des fins pédagogiques et peut être enrichi par :

Analyse sémantique

Génération de code

Interface graphique