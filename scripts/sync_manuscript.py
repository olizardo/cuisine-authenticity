#!/usr/bin/env python3
"""
sync_manuscript.py - In-Place Google Drive / Word Manuscript Synchronization Script
Cuisine Authenticity Project

1. Typography:
   - Sets Alegreya Sans as the universal font across body text, headings, captions, notes, and tables.
   - Preserves Courier New (sz=20 / 10pt, two sizes smaller than 12pt body) specifically for software/package names (brms, cmdstanr, CmdStan, loo, etc.).
   - Strips all non-Alegreya font overrides across all runs, styles, themes, and font tables.
2. Layout & Organization:
   - Preserves clean narrative flow without duplicate blocks.
   - Moves all Tables (Tables 1–3) and Figures (Figures 1–13) to the end of the document,
     ensuring each table and figure is placed next to its corresponding caption and APA note.
   - Embeds publication-grade APA 7th tables and native 6.5-inch aspect-synchronized figures.
"""

import os
import re
import sys
import zipfile
import struct
import xml.etree.ElementTree as ET

DOC_ID = "1qU0OoUbKx_jQ6t1BvkSJ2F2mdbqmJbhqfyRs3SNdrNY"

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
A_NS = "http://schemas.openxmlformats.org/drawingml/2006/main"
R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
WP_NS = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
PIC_NS = "http://schemas.openxmlformats.org/drawingml/2006/picture"
RELS_NS = "http://schemas.openxmlformats.org/package/2006/relationships"

ET.register_namespace('w', W_NS)
ET.register_namespace('a', A_NS)
ET.register_namespace('r', R_NS)
ET.register_namespace('wp', WP_NS)
ET.register_namespace('pic', PIC_NS)

FONT_NAME = "Alegreya Sans"
CODE_FONT = "Courier New"
PACKAGE_NAMES = ["cmdstanr", "CmdStan", "brms", "loo", "bayesplot", "tidybayes", "ggplot2"]

def xml_escape(s):
    if s is None:
        return ""
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")

def get_image_dimensions(image_path):
    if not os.path.exists(image_path):
        return 1950, 1200
    with open(image_path, "rb") as f:
        data = f.read(24)
        if len(data) >= 24 and data.startswith(b'\x89PNG\r\n\x1a\n'):
            w, h = struct.unpack('>II', data[16:24])
            return w, h
    return 1950, 1200

def parse_markdown_table(file_path):
    if not os.path.exists(file_path):
        return [], []
    with open(file_path, "r", encoding="utf-8") as f:
        lines = [line.strip() for line in f if line.strip()]
    table_lines = [line for line in lines if line.startswith("|") and line.endswith("|")]
    if len(table_lines) < 3:
        return [], []
    
    headers = [c.strip() for c in table_lines[0].strip("|").split("|")]
    rows = []
    for line in table_lines[2:]:
        row = [c.strip() for c in line.strip("|").split("|")]
        rows.append(row)
    return headers, rows

def parse_text_runs(text):
    """Splits text into runs of types: 'normal', 'bold', 'italic', 'code'."""
    pkg_re = r"\b(?:" + "|".join(re.escape(p) for p in PACKAGE_NAMES) + r")\b"
    master_re = re.compile(rf"(\*\*[^*]+?\*\*|\*[^*]+?\*|`[^`]+?`|{pkg_re})")
    
    parts = master_re.split(str(text))
    runs = []
    for part in parts:
        if not part:
            continue
        if part.startswith("**") and part.endswith("**") and len(part) >= 4:
            runs.append(("bold", part[2:-2]))
        elif part.startswith("*") and part.endswith("*") and len(part) >= 2:
            runs.append(("italic", part[1:-1]))
        elif part.startswith("`") and part.endswith("`") and len(part) >= 2:
            runs.append(("code", part[1:-1]))
        elif part in PACKAGE_NAMES:
            runs.append(("code", part))
        else:
            runs.append(("normal", part))
    return runs

def format_cell_runs(text):
    """Parses markdown text and returns OpenXML run XML."""
    runs = parse_text_runs(text)
    if not runs:
        return '<w:r><w:t></w:t></w:r>'
    
    res = []
    for r_type, content in runs:
        if r_type == "bold":
            res.append(f'<w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">{xml_escape(content)}</w:t></w:r>')
        elif r_type == "italic":
            res.append(f'<w:r><w:rPr><w:i/></w:rPr><w:t xml:space="preserve">{xml_escape(content)}</w:t></w:r>')
        elif r_type == "code":
            res.append(f'<w:r><w:rPr><w:rFonts w:ascii="{CODE_FONT}" w:hAnsi="{CODE_FONT}" w:cs="{CODE_FONT}"/><w:sz w:val="20"/><w:szCs w:val="20"/></w:rPr><w:t xml:space="preserve">{xml_escape(content)}</w:t></w:r>')
        else:
            res.append(f'<w:r><w:t xml:space="preserve">{xml_escape(content)}</w:t></w:r>')
            
    return "".join(res)

def create_apa_table_xml(headers, rows_data, col_widths=None):
    total_w = 9360  # 6.5 inches portrait width in dxa
    num_cols = len(headers)
    
    if col_widths is None:
        col1_w = int(total_w * 0.36)
        rem_w = total_w - col1_w
        sub_w = int(rem_w / (num_cols - 1))
        col_widths = [col1_w] + [sub_w] * (num_cols - 2)
        col_widths.append(total_w - sum(col_widths))
    
    xml = [
        f'<w:tbl xmlns:w="{W_NS}">',
        f'<w:tblPr>',
        f'<w:tblW w:w="{total_w}" w:type="dxa"/>',
        f'<w:tblBorders>',
        f'<w:top w:val="single" w:sz="8" w:space="0" w:color="000000"/>',
        f'<w:left w:val="none"/>',
        f'<w:bottom w:val="single" w:sz="8" w:space="0" w:color="000000"/>',
        f'<w:right w:val="none"/>',
        f'<w:insideH w:val="none"/>',
        f'<w:insideV w:val="none"/>',
        f'</w:tblBorders>',
        f'<w:tblCellMar>',
        f'<w:top w:w="120" w:type="dxa"/>',
        f'<w:bottom w:w="120" w:type="dxa"/>',
        f'<w:left w:w="160" w:type="dxa"/>',
        f'<w:right w:w="160" w:type="dxa"/>',
        f'</w:tblCellMar>',
        f'</w:tblPr>',
        f'<w:tblGrid>'
    ]
    for w in col_widths:
        xml.append(f'<w:gridCol w:w="{w}"/>')
    xml.append('</w:tblGrid>')
    
    # Header Row
    xml.append('<w:tr><w:trPr><w:tblHeader/><w:cantSplit/></w:trPr>')
    for i, h in enumerate(headers):
        align = "left" if i == 0 else "center"
        clean_h = h.replace("$p_{\\text{WAIC}}$", "p_WAIC").replace("$\\Delta \\text{WAIC}_{\\text{vs. Baseline}}$", "ΔWAIC").replace("$\\Delta \\text{WAIC}$", "ΔWAIC").replace("Models ($k$)", "Models (k)")
        runs_xml = format_cell_runs(clean_h)
        xml.append(
            f'<w:tc><w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/>'
            f'<w:tcBorders><w:bottom w:val="single" w:sz="4" w:space="0" w:color="000000"/></w:tcBorders>'
            f'<w:noWrap/></w:tcPr>'
            f'<w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/>'
            f'<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/><w:jc w:val="{align}"/></w:pPr>'
            f'<w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">{xml_escape(clean_h)}</w:t></w:r></w:p></w:tc>'
        )
    xml.append('</w:tr>')
    
    # Data Rows
    for row in rows_data:
        xml.append('<w:tr><w:trPr><w:cantSplit/></w:trPr>')
        for i, val in enumerate(row):
            align = "left" if i == 0 else "center"
            runs_xml = format_cell_runs(val)
            w_col = col_widths[i] if i < len(col_widths) else col_widths[-1]
            xml.append(
                f'<w:tc><w:tcPr><w:tcW w:w="{w_col}" w:type="dxa"/><w:noWrap/></w:tcPr>'
                f'<w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/>'
                f'<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/><w:jc w:val="{align}"/></w:pPr>'
                f'{runs_xml}</w:p></w:tc>'
            )
        xml.append('</w:tr>')
        
    xml.append('</w:tbl>')
    return "".join(xml)

def make_p_elem(text, bold=False, italic=False, heading_level=None, align=None, before=0, after=120):
    """Creates a formatted OpenXML paragraph ElementTree element strictly inheriting Alegreya Sans (with Courier New for package names)."""
    p = ET.Element(f'{{{W_NS}}}p')
    pPr = ET.SubElement(p, f'{{{W_NS}}}pPr')
    if heading_level:
        pStyle = ET.SubElement(pPr, f'{{{W_NS}}}pStyle')
        pStyle.set(f'{{{W_NS}}}val', f'Heading{heading_level}')
        
    sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
    sp.set(f'{{{W_NS}}}before', str(before))
    sp.set(f'{{{W_NS}}}after', str(after))
    
    ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
    ind.set(f'{{{W_NS}}}left', '0')
    ind.set(f'{{{W_NS}}}right', '0')
    ind.set(f'{{{W_NS}}}firstLine', '0')
    ind.set(f'{{{W_NS}}}hanging', '0')
    
    if align:
        jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
        jc.set(f'{{{W_NS}}}val', align)
        
    runs = parse_text_runs(text)
    for r_type, content in runs:
        r = ET.SubElement(p, f'{{{W_NS}}}r')
        rPr = ET.SubElement(r, f'{{{W_NS}}}rPr')
        
        if r_type == "code":
            rf = ET.SubElement(rPr, f'{{{W_NS}}}rFonts')
            rf.set(f'{{{W_NS}}}ascii', CODE_FONT)
            rf.set(f'{{{W_NS}}}hAnsi', CODE_FONT)
            rf.set(f'{{{W_NS}}}cs', CODE_FONT)
            sz = ET.SubElement(rPr, f'{{{W_NS}}}sz')
            sz.set(f'{{{W_NS}}}val', '20')
            szCs = ET.SubElement(rPr, f'{{{W_NS}}}szCs')
            szCs.set(f'{{{W_NS}}}val', '20')
            if bold: ET.SubElement(rPr, f'{{{W_NS}}}b')
            if italic: ET.SubElement(rPr, f'{{{W_NS}}}i')
        else:
            if bold or r_type == "bold": ET.SubElement(rPr, f'{{{W_NS}}}b')
            if italic or r_type == "italic": ET.SubElement(rPr, f'{{{W_NS}}}i')
            
        t = ET.SubElement(r, f'{{{W_NS}}}t')
        t.text = content
        
    return p

def make_page_break_elem():
    """Builds an OpenXML paragraph containing a clean page break."""
    p = ET.Element(f'{{{W_NS}}}p')
    pPr = ET.SubElement(p, f'{{{W_NS}}}pPr')
    ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
    ind.set(f'{{{W_NS}}}left', '0')
    ind.set(f'{{{W_NS}}}right', '0')
    ind.set(f'{{{W_NS}}}firstLine', '0')
    ind.set(f'{{{W_NS}}}hanging', '0')
    r = ET.SubElement(p, f'{{{W_NS}}}r')
    br = ET.SubElement(r, f'{{{W_NS}}}br')
    br.set(f'{{{W_NS}}}type', 'page')
    return p

def make_drawing_elem(rid, img_path, doc_id_num):
    """Builds a DrawingML ElementTree Element for an image matching Google Docs schema."""
    pw, ph = get_image_dimensions(img_path)
    cx = 5943600  # 6.5 inches in EMUs
    cy = int(round(5943600 * (ph / pw)))
    img_name = os.path.basename(img_path)
    
    p = ET.Element(f'{{{W_NS}}}p')
    pPr = ET.SubElement(p, f'{{{W_NS}}}pPr')
    sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
    sp.set(f'{{{W_NS}}}before', '120')
    sp.set(f'{{{W_NS}}}after', '60')
    
    ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
    ind.set(f'{{{W_NS}}}left', '0')
    ind.set(f'{{{W_NS}}}right', '0')
    ind.set(f'{{{W_NS}}}firstLine', '0')
    ind.set(f'{{{W_NS}}}hanging', '0')
    
    jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
    jc.set(f'{{{W_NS}}}val', 'center')
    
    r = ET.SubElement(p, f'{{{W_NS}}}r')
    drawing = ET.SubElement(r, f'{{{W_NS}}}drawing')
    inline = ET.SubElement(drawing, f'{{{WP_NS}}}inline', {
        'distT': '0', 'distB': '0', 'distL': '114300', 'distR': '114300'
    })
    
    ET.SubElement(inline, f'{{{WP_NS}}}extent', {'cx': str(cx), 'cy': str(cy)})
    ET.SubElement(inline, f'{{{WP_NS}}}effectExtent', {'l': '0', 't': '0', 'r': '0', 'b': '0'})
    ET.SubElement(inline, f'{{{WP_NS}}}docPr', {'id': str(doc_id_num), 'name': img_name})
    
    graphic = ET.SubElement(inline, f'{{{A_NS}}}graphic')
    graphicData = ET.SubElement(graphic, f'{{{A_NS}}}graphicData', {
        'uri': 'http://schemas.openxmlformats.org/drawingml/2006/picture'
    })
    
    pic = ET.SubElement(graphicData, f'{{{PIC_NS}}}pic')
    nvPicPr = ET.SubElement(pic, f'{{{PIC_NS}}}nvPicPr')
    ET.SubElement(nvPicPr, f'{{{PIC_NS}}}cNvPr', {'id': '0', 'name': img_name})
    ET.SubElement(nvPicPr, f'{{{PIC_NS}}}cNvPicPr', {'preferRelativeResize': '0'})
    
    blipFill = ET.SubElement(pic, f'{{{PIC_NS}}}blipFill')
    ET.SubElement(blipFill, f'{{{A_NS}}}blip', {
        f'{{{R_NS}}}embed': rid
    })
    ET.SubElement(blipFill, f'{{{A_NS}}}srcRect', {'l': '0', 't': '0', 'r': '0', 'b': '0'})
    stretch = ET.SubElement(blipFill, f'{{{A_NS}}}stretch')
    ET.SubElement(stretch, f'{{{A_NS}}}fillRect')
    
    spPr = ET.SubElement(pic, f'{{{PIC_NS}}}spPr')
    xfrm = ET.SubElement(spPr, f'{{{A_NS}}}xfrm')
    ET.SubElement(xfrm, f'{{{A_NS}}}off', {'x': '0', 'y': '0'})
    ET.SubElement(xfrm, f'{{{A_NS}}}ext', {'cx': str(cx), 'cy': str(cy)})
    prstGeom = ET.SubElement(spPr, f'{{{A_NS}}}prstGeom', {'prst': 'rect'})
    ET.SubElement(prstGeom, f'{{{A_NS}}}avLst')
    ET.SubElement(spPr, f'{{{A_NS}}}ln')
    
    return p

def sanitize_tree_fonts(tree):
    """Enforces Alegreya Sans inheritance across all elements, preserving Courier New for package names."""
    for r in tree.iter(f'{{{W_NS}}}r'):
        t = r.find(f'{{{W_NS}}}t')
        txt = t.text.strip() if (t is not None and t.text) else ""
        rPr = r.find(f'{{{W_NS}}}rPr')
        
        is_pkg = txt in PACKAGE_NAMES
        
        if is_pkg:
            if rPr is None:
                rPr = ET.SubElement(r, f'{{{W_NS}}}rPr')
            rFonts = rPr.find(f'{{{W_NS}}}rFonts')
            if rFonts is None:
                rFonts = ET.SubElement(rPr, f'{{{W_NS}}}rFonts')
            rFonts.set(f'{{{W_NS}}}ascii', CODE_FONT)
            rFonts.set(f'{{{W_NS}}}hAnsi', CODE_FONT)
            rFonts.set(f'{{{W_NS}}}cs', CODE_FONT)
            sz = rPr.find(f'{{{W_NS}}}sz')
            if sz is None:
                sz = ET.SubElement(rPr, f'{{{W_NS}}}sz')
            sz.set(f'{{{W_NS}}}val', '20')
            szCs = rPr.find(f'{{{W_NS}}}szCs')
            if szCs is None:
                szCs = ET.SubElement(rPr, f'{{{W_NS}}}szCs')
            szCs.set(f'{{{W_NS}}}val', '20')
        else:
            if rPr is not None:
                rFonts = rPr.find(f'{{{W_NS}}}rFonts')
                if rFonts is not None:
                    # Remove rFonts override so it cleanly inherits Alegreya Sans from docDefaults
                    rPr.remove(rFonts)

def sanitize_tree_indentation(tree):
    """Enforces zero indentation across all paragraphs in the document tree."""
    for p in tree.iter(f'{{{W_NS}}}p'):
        pPr = p.find(f'{{{W_NS}}}pPr')
        if pPr is None:
            pPr = ET.SubElement(p, f'{{{W_NS}}}pPr')
        ind = pPr.find(f'{{{W_NS}}}ind')
        if ind is None:
            ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
        ind.set(f'{{{W_NS}}}left', '0')
        ind.set(f'{{{W_NS}}}right', '0')
        ind.set(f'{{{W_NS}}}firstLine', '0')
        ind.set(f'{{{W_NS}}}hanging', '0')

def sync_docx(in_docx, out_docx):
    print(f"Reading input document: {in_docx}")
    with zipfile.ZipFile(in_docx, "r") as zin:
        xml_bytes = zin.read("word/document.xml")
        rels_bytes = zin.read("word/_rels/document.xml.rels")
        styles_bytes = zin.read("word/styles.xml")
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}
    
    # 1. Clean styles.xml to strictly Alegreya Sans and zero indentations
    styles_tree = ET.fromstring(styles_bytes)
    for rFonts in styles_tree.iter(f'{{{W_NS}}}rFonts'):
        rFonts.set(f'{{{W_NS}}}ascii', FONT_NAME)
        rFonts.set(f'{{{W_NS}}}hAnsi', FONT_NAME)
        rFonts.set(f'{{{W_NS}}}cs', FONT_NAME)
        rFonts.set(f'{{{W_NS}}}eastAsia', FONT_NAME)
        
    for docDefaults in styles_tree.iter(f'{{{W_NS}}}docDefaults'):
        pPrDefault = docDefaults.find(f'{{{W_NS}}}pPrDefault')
        if pPrDefault is not None:
            pPr = pPrDefault.find(f'{{{W_NS}}}pPr')
            if pPr is not None:
                ind = pPr.find(f'{{{W_NS}}}ind')
                if ind is None:
                    ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
                ind.set(f'{{{W_NS}}}left', '0')
                ind.set(f'{{{W_NS}}}right', '0')
                ind.set(f'{{{W_NS}}}firstLine', '0')
                ind.set(f'{{{W_NS}}}hanging', '0')

    for style in styles_tree.iter(f'{{{W_NS}}}style'):
        pPr = style.find(f'{{{W_NS}}}pPr')
        if pPr is not None:
            ind = pPr.find(f'{{{W_NS}}}ind')
            if ind is None:
                ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
            ind.set(f'{{{W_NS}}}left', '0')
            ind.set(f'{{{W_NS}}}right', '0')
            ind.set(f'{{{W_NS}}}firstLine', '0')
            ind.set(f'{{{W_NS}}}hanging', '0')
            
    all_files["word/styles.xml"] = ET.tostring(styles_tree, encoding="utf-8", xml_declaration=True)
    print("Sanitized word/styles.xml to Alegreya Sans and zero indentations.")

    # Clean theme1.xml font schemes to Alegreya Sans
    if "word/theme/theme1.xml" in all_files:
        theme_tree = ET.fromstring(all_files["word/theme/theme1.xml"])
        for latin in theme_tree.iter(f'{{{A_NS}}}latin'):
            latin.set("typeface", FONT_NAME)
        all_files["word/theme/theme1.xml"] = ET.tostring(theme_tree, encoding="utf-8", xml_declaration=True)
        print("Sanitized word/theme/theme1.xml to Alegreya Sans.")

    # Clean fontTable.xml to strictly Alegreya Sans and Courier New
    clean_font_table = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
        f'<w:fonts xmlns:w="{W_NS}" xmlns:r="{R_NS}">\n'
        f'  <w:font w:name="{FONT_NAME}">\n'
        f'    <w:family w:val="swiss"/>\n'
        f'    <w:pitch w:val="variable"/>\n'
        f'  </w:font>\n'
        f'  <w:font w:name="{CODE_FONT}">\n'
        f'    <w:family w:val="modern"/>\n'
        f'    <w:pitch w:val="fixed"/>\n'
        f'  </w:font>\n'
        f'</w:fonts>'
    )
    all_files["word/fontTable.xml"] = clean_font_table.encode("utf-8")
    print("Sanitized word/fontTable.xml.")
    
    # 2. Prepare relationships for all 13 Figures
    ET.register_namespace('', RELS_NS)
    root_rels = ET.fromstring(rels_bytes)
    
    existing_nums = []
    for e in root_rels:
        r_id = e.get('Id', '')
        m = re.match(r'rId(\d+)', r_id)
        if m:
            existing_nums.append(int(m.group(1)))
    max_rid_num = max(existing_nums) if existing_nums else 8
    
    figures_data = [
        (1, "Plots/model_fit_comparison.png", "image1.png",
         "Figure 1. Bayesian Model Fit Comparison Relative to Baseline Model 1.",
         "Note: Baseline reference model is Cultural Capital Strict RI (WAIC = 55,310.59). Negative ΔWAIC indicates superior out-of-sample predictive fit."),
        (2, "Plots/cuisine_random_effects.png", "image2.png",
         "Figure 2. Baseline Cuisine Authenticity Hierarchy: Cross-Specification Consensus.",
         "Note: Cuisine-specific random intercepts estimated across baseline specifications, capturing the underlying domestic elder versus professional chef baseline ordering."),
        (3, "Plots/fixed_effects_stability_forest.png", "image3.png",
         "Figure 3. Cross-Specification Parameter Stability Envelope Across All 18 Models.",
         "Note: Half-eye posterior distributions synthesize draws across all eighteen models, with point medians, 80% and 95% credible intervals, and individual model median tick marks (+)."),
        (4, "Plots/ideology_cs_midpoint_effects.png", "image4.png",
         "Figure 4. Political Ideology Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Social conservatism exhibits an escalating positive contrast trajectory across the upper scale, reaching its highest contrast at Category 7 (Professional Chef: median ≈ +0.45). In contrast, economic conservatism exerts a moderating influence, concentrating ratings around the neutral midpoint."),
        (5, "Plots/cultural_cs_midpoint_effects.png", "image5.png",
         "Figure 5. Cultural Capital Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Educational attainment systematically increases the likelihood of rating cuisines toward the professional chef anchor (Categories 5–7). Parental education dampens extreme categories, while childhood arts exposure shows diffuse intervals spanning zero when adult credentials are held constant."),
        (6, "Plots/practices_cs_midpoint_effects.png", "image6.png",
         "Figure 6. Dining Practices Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Adult highbrow arts participation suppresses ratings for domestic elder categories while elevating upper restaurant categories. Fine dining frequency exhibits an escalating monotonic trajectory across the upper scale, peaking at Category 7 (Chef vs. 4: median = +0.69, 95% CrI [0.45, 0.95], P > 0 = 100%)."),
        (7, "Plots/dispositions_cs_midpoint_effects.png", "image7.png",
         "Figure 7. Bourdieu Taste Dispositions Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Respondents prioritizing exotic and authentic flavors exhibit an elevation across all domestic elder categories relative to the midpoint (Category 1: median = +0.39, 95% CrI [0.20, 0.59]), alongside depression of the extreme chef category (Category 7: -0.15)."),
        (8, "Plots/cosmopolitan_cs_midpoint_effects.png", "image8.png",
         "Figure 8. Cosmopolitan Capital Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Friendship network diversity credibly increases the log-odds of selecting professional chef categories relative to the neutral midpoint (Category 5: +0.21; Category 6: +0.28; Category 7: +0.31), while depressing domestic elder categories."),
        (9, "Plots/rs_cuisine_slopes_ideology.png", "image9.png",
         "Figure 9. Cuisine Random Slopes: Ideology (Social vs. Economic Conservatism), Partitioned by Consecration Tiers.",
         "Note: Social conservatism acts as a countervailing force, generating positive slopes across subaltern cuisines (Native American median = +0.30, Pakistani median = +0.22, Lebanese median = +0.21, Jamaican median = +0.21, Mexican median = +0.21, Nigerian median = +0.18) that pull peripheral traditions toward the professional chef boundary."),
        (10, "Plots/rs_cuisine_slopes_cultural.png", "image10.png",
         "Figure 10. Cuisine Random Slopes: Cultural Capital (Education, Parental Education, Childhood Arts), Partitioned by Consecration Tiers.",
         "Note: Respondent education demonstrates positive slopes across cuisines, with traditions such as Native American (median = +0.19), Italian (+0.15), Mexican (+0.15), and Swedish (+0.14) showing positive shifts toward chef craftsmanship."),
        (11, "Plots/rs_cuisine_slopes_practices.png", "image11.png",
         "Figure 11. Cuisine Random Slopes: Dining Practices (Highbrow Arts, Fine Dining, Fast Food), Partitioned by Consecration Tiers.",
         "Note: Adult highbrow arts attendance and fine dining frequency exert positive pro-chef pulls across both haute and subaltern culinary traditions—most notably on Native American (median = +0.20), Mexican (median = +0.18), and Italian cuisines (median = +0.16)."),
        (12, "Plots/rs_cuisine_slopes_dispositions.png", "image12.png",
         "Figure 12. Cuisine Random Slopes: Taste Dispositions (Exotic & Authentic vs. Conventional & Familiar), Partitioned by Consecration Tiers.",
         "Note: Liking 'exotic and authentic' food credibly reinforces the domestic elder orientation across nearly all subaltern and non-Western cuisines—including Nigerian (-0.13), Peruvian (-0.13), Moroccan (-0.12), Ethiopian (-0.12), Pakistani (-0.11), Jamaican (-0.10), Lebanese (-0.10), and Korean (-0.10)."),
        (13, "Plots/rs_cuisine_slopes_cosmopolitan.png", "image13.png",
         "Figure 13. Cuisine Random Slopes: Cosmopolitan Capital (Global Citizen Identity, Friendship Network Diversity), Partitioned by Consecration Tiers.",
         "Note: Friendship network diversity and global citizen identity elevate marginalized cuisines into fine dining legitimacy (Native American +0.15 and +0.08, Italian +0.13, Mexican +0.08 and +0.06) while de-centering Western haute culinary hegemony (French -0.02).")
    ]
    
    fig_rids = {}
    current_rid_num = max_rid_num
    
    for f_num, f_path, f_target_name, _, _ in figures_data:
        current_rid_num += 1
        r_id = f"rId{current_rid_num}"
        fig_rids[f_num] = r_id
        
        if os.path.exists(f_path):
            with open(f_path, "rb") as f_img:
                all_files[f"word/media/{f_target_name}"] = f_img.read()
        
        rel_elem = ET.Element(f"{{{RELS_NS}}}Relationship", {
            "Id": r_id,
            "Type": "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
            "Target": f"media/{f_target_name}"
        })
        root_rels.append(rel_elem)
        print(f"Registered {r_id} -> media/{f_target_name}")
        
    all_files["word/_rels/document.xml.rels"] = ET.tostring(root_rels, encoding="utf-8", xml_declaration=True)
    
    # 3. Parse and Clean Body Text
    doc_tree = ET.fromstring(xml_bytes)
    body = doc_tree.find(f'{{{W_NS}}}body')
    orig_elements = list(body)
    
    # Identify sectPr
    sectPr = None
    for elem in orig_elements:
        if elem.tag.endswith('sectPr'):
            sectPr = elem
            break
            
    clean_body_elements = []
    
    # Step 1: Narrative up to Results
    # Find start of Results section dynamically
    results_idx = None
    for idx, elem in enumerate(orig_elements):
        txt = ''.join(elem.itertext()).strip()
        if txt in ["Results", "RESULTS", "5. Results", "5. RESULTS"]:
            results_idx = idx
            break
    if results_idx is None:
        results_idx = 71
        
    for i in range(results_idx):
        elem = orig_elements[i]
        txt = ''.join(elem.itertext()).strip()
        if txt.startswith("Table 1.") or txt.startswith("Table 1:") or (elem.tag.endswith("tbl") and "Cuisine Consecration Hierarchy" in txt):
            continue
        clean_body_elements.append(elem)
        
    # Step 2: Results section text (clean narrative only, without inline tables or figure drawings)
    clean_body_elements.append(make_p_elem("Results", heading_level=1, before=240, after=120))
    clean_body_elements.append(make_p_elem("Bayesian Model Fit and Taxonomy Comparison", heading_level=2, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "We evaluated model performance using the Watanabe-Akaike Information Criterion (WAIC; Watanabe 2010), implemented via the `loo` package (Vehtari et al. 2017), across all eighteen completed specifications in the factorial taxonomy and omnibus meta architecture. As presented in Table 2, Model 18 (Omnibus Meta Relaxed RS) achieves the overall superior out-of-sample predictive fit (WAIC = 53,934.59, ΔWAIC = -1,375.99 relative to Baseline Model 1)."
    ))
    clean_body_elements.append(make_p_elem(
        "The model comparisons in Figure 1 reveal three main insights. First, relaxing the proportional odds threshold constraint yields substantial predictive gains across all theoretical domains, with the largest single-domain improvements observed in Taste Dispositions (ΔWAIC = -962.33) and Dining Practices (ΔWAIC = -863.31). This means people with different levels of cultural capital, taste dispositions, political ideologies, and so forth treat the ordinal scale category boundaries differently, and those differences matter. Second, within each substantive domain, incorporating crossed random slopes on cuisines provides noticeable predictive improvements over random intercepts alone, reflecting meaningful heterogeneity in how cultural and ideological dispositions operate across culinary categories. Third, estimating all fourteen substantive mechanisms simultaneously with both relaxed category-specific transitions and crossed cuisine random slopes in the full omnibus meta specification (Model 18) yields the single best predictive performance across the entire project (WAIC = 53,934.59), representing a decisive 1,375.99-point out-of-sample predictive gain over the baseline model and outperforming the random-intercepts meta model by an additional -376.76 WAIC points. This demonstrates that cultural and cosmopolitan capital, food taste dispositions, political ideology, and cultural practices operate jointly in predicting perceptions of authenticity across different cuisines."
    ))
    
    clean_body_elements.append(make_p_elem("Baseline Authenticity by Cuisine: Cross-Specification Consensus", heading_level=2, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "To establish the baseline culinary hierarchy, we estimated cuisine-specific random intercepts across model specifications. As shown in Figure 2, the baseline ratings confirm a robust three-tiered consecration hierarchy: Consecrated Western and East Asian cuisines (French, Japanese, Swedish) anchor the professional chef pole, while Subaltern and peripheral cuisines (Native American, Jamaican, Nigerian, Ethiopian, Mexican, Pakistani) strongly anchor the domestic elder authenticity pole."
    ))
    clean_body_elements.append(make_p_elem(
        "The Figure provides strong support for H1, confirming a robust three-tiered structure of culinary consecration."
    ))
    
    clean_body_elements.append(make_p_elem("Cross-Specification Consensus and Fixed Effects Stability Envelope", heading_level=2, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "To assess parameter reliability and guard against single-specification dependency, posterior distributions are synthesized across the complete factorial taxonomy and meta architecture (Table 3 and Figure 3). Category-specific draws are integrated into a marginal transition-averaged parameter per MCMC iteration, capturing the overall directional push across the entire response continuum."
    ))
    
    clean_body_elements.append(make_p_elem("Substantive Mechanisms & Theoretical Hypotheses", heading_level=2, before=240, after=120))
    clean_body_elements.append(make_p_elem(
        "Evaluating empirical hypotheses through the cross-specification stability envelope provides rigorous multi-model tests while protecting against specification dependency."
    ))
    
    clean_body_elements.append(make_p_elem("Political Ideology: Differentiation and Asymmetry (Hypotheses 2 & 3)", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "The empirical findings provide consistent support for both the Ideological Differentiation Hypothesis (H2) and the Ideological Asymmetry Hypothesis (H3). Across all eighteen model specifications, social conservatism is a positive predictor of endorsing professional chef mastery, with a grand mean effect of +0.148 (median range [+0.107, +0.193], P(β > 0) ≥ 99.1%). In contrast, progressive and socially liberal orientations systematically anchor evaluations in domestic elder authenticity."
    ))
    clean_body_elements.append(make_p_elem(
        "Conversely, economic conservatism exhibits an attenuated coefficient tightly centered near zero across all specifications (grand mean β = -0.022, median range [-0.043, +0.003], 95% CrI [-0.151, +0.094]). Direct contrast testing demonstrates that the posterior difference between social and economic conservatism is credibly positive (P(β_social - β_economic > 0) = 99.3%). This asymmetry suggests that culinary schemas are organized around symbolic, cultural, and moral boundaries rather than fiscal or market preferences."
    ))
    
    clean_body_elements.append(make_p_elem("Cultural Capital Dual Mechanism: Institutional Distinction vs. Embodied Socialization (Hypotheses 4 & 6)", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "Evaluating cultural capital across specifications indicates a structural decoupling between institutionalized credentials and embodied early socialization, supporting Hypotheses 4 and 6. Formal educational attainment systematically shifts evaluations toward professional restaurant mastery across all eighteen models (grand mean β = +0.094, median range [+0.053, +0.123], P(β > 0) ≥ 93.3%), reflecting the acquisition of highbrow culinary schemas that valorize formal gastronomic technique."
    ))
    clean_body_elements.append(make_p_elem(
        "In contrast, once adult cultural consumption and education are accounted for, childhood arts socialization consistently anchors taste in domestic elder authenticity (grand mean β = -0.053, median range [-0.094, -0.022], P(β < 0) ≥ 99.9%). Early embodied socialization appears to root aesthetic distinction in familial heritage, counterbalancing adult institutionalization. Furthermore, supporting Hypothesis 6, household income (β = +0.020) and parental education (β = -0.030) span zero across specifications, confirming that culinary distinction operates primarily as symbolic cultural capital rather than a direct reflection of material wealth."
    ))
    
    clean_body_elements.append(make_p_elem("Behavioral Dining Practices and Active Cultural Consumption (Hypothesis 7)", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "Active adult behavioral consumption reinforces institutional gastronomic distinction, providing support for Hypothesis 7. Engaging regularly in highbrow arts activities, such as museums, opera, theater, and dance, elevates pro-chef evaluations (grand mean β = +0.117, median range [+0.102, +0.142], P(β > 0) ≥ 98.6%). Similarly, frequent dining at fine dining table-service restaurants reliably increases pro-chef ratings (grand mean β = +0.111, median range [+0.087, +0.125], P(β > 0) ≥ 98.2%). In contrast, fast food consumption exhibits a neutral global association (β = +0.038) spanning zero."
    ))
    
    clean_body_elements.append(make_p_elem("Bourdieu Taste Dispositions and Authenticity Construct Validation (Hypothesis 8)", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "The taste disposition models provide strong construct validation for the authenticity continuum, supporting Hypothesis 8. Expressing a strong preference for exotic and authentic dishes credibly predicts domestic elder authenticity across specifications (grand mean β = -0.100, median range [-0.126, -0.085], P(β < 0) ≥ 99.4%). This pattern indicates that individuals seeking authentic culinary experiences specifically locate authenticity in home elder preparation rather than upscale restaurant reinvention. Preferences for familiar comfort foods (β = +0.028), light dishes (β = -0.031), and rich dishes (β = -0.029) remain centered near zero across specifications."
    ))
    
    clean_body_elements.append(make_p_elem("Cosmopolitan Capital and Bridging Social Networks (Hypothesis 5)", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "Cosmopolitan openness and bridging social ties foster appreciation for formal culinary mastery across diverse traditions, providing support for Hypothesis 5. Having close friendships across multiple ethnoracial minority groups credibly increases pro-chef ratings (grand mean β = +0.045, median range [+0.012, +0.061], P(β > 0) ≥ 96.6%). Self-identification as a global citizen exhibits a modest positive orientation (β = +0.034, median range [+0.031, +0.038])."
    ))
    
    clean_body_elements.append(make_p_elem("Sociodemographic and Ethnoracial Anchors", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "Sociodemographic indicators show consistent baseline patterns. Women exhibit a reliable orientation toward traditional domestic elder authenticity relative to men across all eighteen specifications (grand mean β = -0.175, median range [-0.200, -0.155], P(β < 0) ≥ 99.6%). Older respondents similarly orient toward domestic elder craftsmanship (grand mean β = -0.048, median range [-0.068, -0.027], P(β < 0) ≥ 98.4%). Among ethnoracial identities, Mixed White identification exhibits a domestic elder orientation (β = -0.257, P < 0 ≥ 97.4%), while Black, Hispanic/Latino, Asian, and Native American identifications exhibit wider credible intervals that span zero across specifications."
    ))
    
    clean_body_elements.append(make_p_elem("Category-Specific Midpoint Contrast Analyses", heading_level=2, before=240, after=120))
    clean_body_elements.append(make_p_elem(
        "To test non-proportional threshold dynamics and analyze how substantive predictors shift probability mass across individual response categories relative to the neutral scale midpoint (Y = 4: 'Equal / One is not better than the other'), category-specific adjacent category models estimate threshold-varying contrast log-odds across all scale levels (Figures 4–8)."
    ))
    clean_body_elements.append(make_p_elem("Political Ideology Midpoint Contrasts", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As depicted in Figure 4, social conservatism exhibits an escalating positive contrast trajectory across the upper scale, reaching its highest contrast at Category 7 (Professional Chef: median ≈ +0.45). In contrast, economic conservatism exerts a moderating influence, concentrating ratings around the neutral midpoint."
    ))
    clean_body_elements.append(make_p_elem("Cultural Capital Midpoint Contrasts", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As shown in Figure 5, educational attainment systematically increases the likelihood of rating cuisines toward the professional chef anchor (Categories 5–7). Parental education dampens extreme categories, while childhood arts exposure shows diffuse intervals spanning zero when adult credentials are held constant."
    ))
    clean_body_elements.append(make_p_elem("Behavioral Dining Practices Midpoint Contrasts", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As illustrated in Figure 6, adult highbrow arts participation suppresses ratings for domestic elder categories while elevating upper restaurant categories. Fine dining frequency exhibits an escalating monotonic trajectory across the upper scale, peaking at Category 7 (Chef vs. 4: median = +0.69, 95% CrI [0.45, 0.95], P > 0 = 100%)."
    ))
    clean_body_elements.append(make_p_elem("Bourdieu Taste Dispositions Midpoint Contrasts", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As presented in Figure 7, respondents prioritizing exotic and authentic flavors exhibit an elevation across all domestic elder categories relative to the midpoint (Category 1: median = +0.39, 95% CrI [0.20, 0.59]), alongside depression of the extreme chef category (Category 7: -0.15)."
    ))
    clean_body_elements.append(make_p_elem("Cosmopolitan Capital Midpoint Contrasts", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As shown in Figure 8, friendship network diversity credibly increases the log-odds of selecting professional chef categories relative to the neutral midpoint (Category 5: +0.21; Category 6: +0.28; Category 7: +0.31), while depressing domestic elder categories."
    ))
    
    clean_body_elements.append(make_p_elem("Cuisine-Specific Heterogeneity and Consecration Hierarchies", heading_level=2, before=240, after=120))
    clean_body_elements.append(make_p_elem(
        "To test whether demographic, ideological, and cultural associations vary systematically across culinary traditions, crossed random slope coefficients were estimated for each focal continuous predictor across substantive model domains (Figures 9–13). Combining global fixed effects with cuisine-level random adjustments reveals the net slope for each culinary tradition across the three baseline consecration tiers: Consecrated (French, Japanese, Swedish), Intermediate (Korean, Italian, Vietnamese, Peruvian, Moroccan, Lebanese), and Subaltern (Pakistani, Mexican, Ethiopian, Jamaican, Nigerian, Native American)."
    ))
    clean_body_elements.append(make_p_elem("Domain 1: Political Ideology Slopes Across Cuisines", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As shown in Figure 9, social conservatism acts as a countervailing force, generating positive slopes across subaltern cuisines (Native American median = +0.30, Pakistani median = +0.22, Lebanese median = +0.21, Jamaican median = +0.21, Mexican median = +0.21, Nigerian median = +0.18) that pull peripheral traditions toward the professional chef boundary."
    ))
    clean_body_elements.append(make_p_elem("Domain 1: Cultural Capital Slopes Across Cuisines", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As illustrated in Figure 10, respondent education demonstrates positive slopes across cuisines, with traditions such as Native American (median = +0.19), Italian (+0.15), Mexican (+0.15), and Swedish (+0.14) showing positive shifts toward chef craftsmanship."
    ))
    clean_body_elements.append(make_p_elem("Domain 2: Behavioral Dining Practices Slopes Across Cuisines", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As shown in Figure 11, adult highbrow arts attendance and fine dining frequency exert positive pro-chef pulls across both haute and subaltern culinary traditions—most notably on Native American (median = +0.20), Mexican (median = +0.18), and Italian cuisines (median = +0.16)."
    ))
    clean_body_elements.append(make_p_elem("Domain 3: Bourdieu Taste Dispositions Slopes Across Cuisines", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As depicted in Figure 12, liking 'exotic and authentic' food credibly reinforces the domestic elder orientation across nearly all subaltern and non-Western cuisines—including Nigerian (-0.13), Peruvian (-0.13), Moroccan (-0.12), Ethiopian (-0.12), Pakistani (-0.11), Jamaican (-0.10), Lebanese (-0.10), and Korean (-0.10)."
    ))
    clean_body_elements.append(make_p_elem("Domain 4: Cosmopolitan Capital and Social Networks Slopes Across Cuisines", heading_level=3, before=180, after=80))
    clean_body_elements.append(make_p_elem(
        "As presented in Figure 13, friendship network diversity and global citizen identity elevate marginalized cuisines into fine dining legitimacy (Native American +0.15 and +0.08, Italian +0.13, Mexican +0.08 and +0.06) while de-centering Western haute culinary hegemony (French -0.02)."
    ))
    
    # Step 3: Discussion and subsequent author sections from original document
    disc_idx = None
    for idx, elem in enumerate(orig_elements):
        txt = ''.join(elem.itertext()).strip()
        if txt == "DISCUSSION" or txt.startswith("DISCUSSION"):
            disc_idx = idx
            break
            
    if disc_idx is not None:
        for idx in range(disc_idx, len(orig_elements)):
            elem = orig_elements[idx]
            txt = ''.join(elem.itertext()).strip()
            # Stop if encountering TABLES or FIGURES section headers from a previous sync
            if txt in ["TABLES", "FIGURES"] or txt.startswith("Table 1.") or txt.startswith("Table 2.") or txt.startswith("Table 3.") or txt.startswith("Figure 1."):
                break
            if not elem.tag.endswith('sectPr'):
                clean_body_elements.append(elem)
                
    # -------------------------------------------------------------
    # 4. Construct Tables Section (at the end of document)
    # -------------------------------------------------------------
    clean_body_elements.append(make_page_break_elem())
    clean_body_elements.append(make_p_elem("TABLES", heading_level=1, before=360, after=180))
    
    # Table 1
    h1, r1 = parse_markdown_table("cache/table_hypotheses.md")
    clean_body_elements.append(make_p_elem("Table 1. Summary of Theoretical Hypotheses.", bold=True, before=240, after=80))
    if h1 and r1:
        tbl1_xml = create_apa_table_xml(h1, r1, [1200, 2760, 5400])
        clean_body_elements.append(ET.fromstring(tbl1_xml))
    clean_body_elements.append(make_page_break_elem())
    
    # Table 2
    h2, r2 = parse_markdown_table("cache/table_fit_comparison.md")
    clean_body_elements.append(make_p_elem("Table 2. Consolidated Bayesian Model Fit Comparison Across the Full Factorial Taxonomy and Meta Architecture.", bold=True, before=240, after=80))
    if h2 and r2:
        tbl2_xml = create_apa_table_xml(h2, r2, [2100, 1500, 1200, 1200, 1100, 660, 700, 900])
        clean_body_elements.append(ET.fromstring(tbl2_xml))
    clean_body_elements.append(make_p_elem(
        "Note: Baseline reference model is Cultural Capital Strict RI (WAIC = 55,310.59). Negative ΔWAIC indicates superior out-of-sample predictive fit.",
        italic=True, before=60, after=120
    ))
    clean_body_elements.append(make_page_break_elem())
    
    # Table 3
    h3, r3 = parse_markdown_table("cache/table_parameter_stability.md")
    clean_body_elements.append(make_p_elem("Table 3. Cross-Specification Parameter Stability Envelope Across All 18 Models (Ordered by Effect Size).", bold=True, before=240, after=80))
    if h3 and r3:
        tbl3_xml = create_apa_table_xml(h3, r3, [2400, 1400, 800, 960, 1400, 1200, 1200])
        clean_body_elements.append(ET.fromstring(tbl3_xml))
    clean_body_elements.append(make_p_elem(
        "Note: Marginal transition-averaged parameter summary synthesizing posterior distributions across all 18 models. Credibility indicates directional posterior mass ≥ 95%.",
        italic=True, before=60, after=120
    ))
    clean_body_elements.append(make_page_break_elem())
    
    # -------------------------------------------------------------
    # 5. Construct Figures Section (at the end of document)
    # -------------------------------------------------------------
    clean_body_elements.append(make_p_elem("FIGURES", heading_level=1, before=360, after=180))
    
    for f_num, f_path, _, caption_txt, note_txt in figures_data:
        rid = fig_rids[f_num]
        clean_body_elements.append(make_p_elem(caption_txt, bold=True, before=240, after=80))
        clean_body_elements.append(make_drawing_elem(rid, f_path, 100 + f_num))
        clean_body_elements.append(make_p_elem(note_txt, italic=True, before=60, after=240))
        
    # Re-attach sectPr
    if sectPr is not None:
        clean_body_elements.append(sectPr)
        
    # 6. Rebuild body with clean elements
    body.clear()
    for elem in clean_body_elements:
        body.append(elem)
        
    # Sanitize all fonts and indentations across the document tree
    sanitize_tree_fonts(doc_tree)
    sanitize_tree_indentation(doc_tree)
    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)
    
    # Also sanitize footer
    if "word/footer1.xml" in all_files:
        footer_tree = ET.fromstring(all_files["word/footer1.xml"])
        sanitize_tree_fonts(footer_tree)
        sanitize_tree_indentation(footer_tree)
        all_files["word/footer1.xml"] = ET.tostring(footer_tree, encoding="utf-8", xml_declaration=True)
        
    print(f"Writing updated document: {out_docx}")
    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)
            
    print("Document successfully transformed and synchronized!")

if __name__ == "__main__":
    in_doc = sys.argv[1] if len(sys.argv) > 1 else "draft_live.docx"
    out_doc = sys.argv[2] if len(sys.argv) > 2 else "draft_updated.docx"
    sync_docx(in_doc, out_doc)
