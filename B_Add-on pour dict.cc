// ==UserScript==
// @name         Easy copy & listen for dict.cc
// @namespace    http://tampermonkey.net/
// @version      0.6
// @description  Click on the clipboard to hear a word and copy it to your clipboard. Alt + A to access the search bar quickly.
// @author       mister-stirling
// @match        https://www.dict.cc/*
// @match        https://www.linguee.fr/*
// @match        https://de-en.dict.cc/*
// @match        https://defr.dict.cc/*
// @grant        GM_setClipboard
// ==/UserScript==

(function () {
    'use strict';

    // changes position of dict.cc main content to the left (more comfy to look at and use)
    document.getElementById('maincontent')?.style && (document.getElementById('maincontent').style.marginLeft = '100px');

const observer = new MutationObserver(() => {
    const el = document.getElementById('maincontent');
    if (el) el.style.marginLeft = '100px';
});

observer.observe(document.body, { childList: true, subtree: true });

    // QoL improvements

    window.addEventListener('load', () => {

            // --- AUTO-REMOVE SHIT ADVERTISEMENT DIVS ---
    document.querySelectorAll('div[style*="height:600px"][style*="width:300px"]').forEach(el => el.remove());
    // ---------------------------------

        // Raccourci clavier Alt + A pour afficher #sinp si caché
        window.addEventListener('keydown', (e) => {
            if (e.altKey && e.key.toLowerCase() === 'a') {
                const sinp = document.getElementById('sinp');
                if (sinp && sinp.style.display === 'none') {
                    sinp.style.display = '';
                    e.preventDefault();
                }
            }
        });

        const rows = Array.from(document.querySelectorAll('tr[id^="tr"]'));

        rows.forEach(tr => {
            if (tr.querySelector('.copy-row-button')) return;

            const tds = tr.querySelectorAll('td.td7nl');
            if (tds.length < 2) return;

            const english = tds[0].textContent.trim();
            const germanRaw = tds[1].textContent.trim();

            // Extraction du nombre de votes en début de texte allemand
            const voteMatch = germanRaw.match(/^(\d+)(.*)/);
            const votes = voteMatch ? voteMatch[1] : '';
            const german = voteMatch ? voteMatch[2].trim() : germanRaw;

            const formatted = votes
                ? `${german} :  : ${english} | ${votes}`
                : `${german} :  : ${english}`;

            // Création du bouton
            const btn = document.createElement('button');
            btn.textContent = '📋';
            btn.title = 'Copy formatted pair and show editor';
            btn.className = 'copy-row-button';
            Object.assign(btn.style, {
                marginRight: '6px',
                fontSize: '1em',
                padding: '2px 6px',
                lineHeight: '1.2',
                cursor: 'pointer'
            });

            btn.addEventListener('click', (e) => {
                e.stopPropagation();
                e.preventDefault();

                GM_setClipboard(formatted);

                btn.textContent = '✅';
                setTimeout(() => btn.textContent = '📋', 1000);

                // Cache #sinp pour éviter le focus automatique sur la page
                const sinp = document.getElementById('sinp');
                if (sinp) sinp.style.display = 'none';

                // Lancer la prononciation si fonction disponible
                const trId = tr.id.substr(2, 4);
                if (typeof cmclick === 'function') cmclick(trId, 2);

                // Gestion du textarea éditeur
                let next = tr.nextElementSibling;
                if (next && next.classList.contains('copy-editor-row')) {
                    // Focus sur textarea existant
                    const existingTextarea = next.querySelector('textarea');
                    if (existingTextarea) existingTextarea.focus();
                } else {
                    // Création d’une nouvelle ligne avec textarea
                    const editorTr = document.createElement('tr');
                    editorTr.className = 'copy-editor-row';

                    const colspan = tr.children.length;
                    const editorTd = document.createElement('td');
                    editorTd.colSpan = colspan;
                    Object.assign(editorTd.style, {
                        padding: '4px 8px',
                        background: '#f9f9f9'
                    });

                    const textarea = document.createElement('textarea');
                    textarea.value = formatted;
                    Object.assign(textarea.style, {
                        width: '100%',
                        height: '60px',
                        fontSize: '14px',
                        fontFamily: 'monospace',
                        resize: 'vertical'
                    });

                    editorTd.appendChild(textarea);
                    editorTr.appendChild(editorTd);
                    tr.parentNode.insertBefore(editorTr, tr.nextSibling);

                    textarea.focus();

                    // Positionner le curseur juste après le premier ':'
                    const firstColonIndex = textarea.value.indexOf(':');
                    if (firstColonIndex !== -1) {
                        const pos = firstColonIndex + 2;
                        textarea.setSelectionRange(pos, pos);
                    }

                    // Maintenir focus sur textarea pendant 1s pour garantir le focus
                    const focusInterval = setInterval(() => textarea.focus(), 50);
                    setTimeout(() => clearInterval(focusInterval), 1000);
                }
            });

            // Insertion du bouton en début de la cellule anglaise
            tds[0].insertBefore(btn, tds[0].firstChild);
        });
    });
})();
