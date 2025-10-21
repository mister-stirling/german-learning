import fitz  # PyMuPDF

# Ouvre le PDF
pdf_path = "xxx"
doc = fitz.open(pdf_path)

# Dictionnaire pour stocker le nombre de mots par page
word_counts = {}

# Parcours des pages
for page_num in range(len(doc)):
    page = doc.load_page(page_num)
    text = page.get_text("text")  # extrait le texte brut
    words = text.split()
    word_counts[page_num + 1] = len(words)

# Affiche les résultats
for page, count in word_counts.items():
    print(f"Page {page}: {count} mots")

# (Optionnel) somme totale
total = sum(word_counts.values())
print(f"\nTotal: {total} mots dans le document")
