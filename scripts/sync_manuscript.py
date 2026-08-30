#!/usr/bin/env python3
"""
sync_manuscript.py - In-Place Google Drive / Word Manuscript Synchronization Script
Cuisine Authenticity Project

Strictly adheres to global AGENTS guidelines:
1. Typography & Font Normalization:
   - Sets Alegreya Sans as universal base font across body text, headings, captions, notes, and tables.
   - Preserves Courier New (sz=20 / 10pt) specifically for software/package names (brms, cmdstanr, CmdStan, loo, etc.).
   - Strips non-Alegreya overrides across runs, styles, themes, and font tables.
2. Document Styles & Paragraph Normalization (Section 8 of AGENTS.md):
   - Normal Body Prose: inherits Normal style with 0.5 in (720 dxa) first-line indent, full justification (jc="both").
   - Headings (H1, H2, H3): zero indent, left-aligned, native heading styles without direct run overrides.
   - Table Captions: zero indent, bold, left-aligned (jc="left"), tight spacing (before=80, after=80).
   - Figure Captions: zero indent, bold, justified (jc="both"), tight spacing (before=60, after=160).
   - Notes: strictly styled as Normal (never Heading4), zero indent, tight spacing (before=80, after=120), justified (jc="both").
   - Drawings: zero indent, center-aligned (jc="center"), dual DrawingML extents synchronized to 6.5 in width.
3. Universal APA 7th Table Styling:
   - Full 6.5 in portrait width (9360 dxa), proportional column allocation.
   - Anti-word-break (<w:suppressAutoHyphens/>) and <w:noWrap/> on numeric cells.
   - Pagination protection (<w:cantSplit/> and <w:tblHeader/>).
   - Cell padding 120 dxa (6pt) top/bottom, 160 dxa (8pt) left/right.
   - Standard APA borders (1pt top/bottom, 0.5pt header underline, 0pt vertical).
4. Workspace Hygiene & File Cleanup.
"""

import os
import re
import sys
import glob
import zipfile
import struct
import xml.etree.ElementTree as ET

DOC_ID = "1qU0OoUbKx_jQ6t1BvkSJ2F2mdbqmJbhqfyRs3SNdrNY"

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
A_NS = "http://schemas.openxmlformats.org/drawingml/2006/main"
R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
M_NS = "http://schemas.openxmlformats.org/officeDocument/2006/math"
WP_NS = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
PIC_NS = "http://schemas.openxmlformats.org/drawingml/2006/picture"
RELS_NS = "http://schemas.openxmlformats.org/package/2006/relationships"

NS_MAP = {
    'w': W_NS,
    'm': M_NS,
    'a': A_NS,
    'r': R_NS,
    'wp': WP_NS,
    'pic': PIC_NS,
    'rel': RELS_NS
}

ET.register_namespace('w', W_NS)
ET.register_namespace('m', M_NS)
ET.register_namespace('a', A_NS)
ET.register_namespace('r', R_NS)
ET.register_namespace('wp', WP_NS)
ET.register_namespace('pic', PIC_NS)

FONT_NAME = "Alegreya Sans"
CODE_FONT = "Courier New"
PACKAGE_NAMES = ["cmdstanr", "CmdStan", "brms", "loo", "bayesplot", "tidybayes", "ggplot2"]

H1_TITLES = [
    "INTRODUCTION", "BACKGROUND", "HYPOTHESES", "DATA AND METHODS",
    "Results", "RESULTS", "5. Results", "5. RESULTS",
    "DISCUSSION", "TABLES", "FIGURES", "NOTES"
]

H2_TITLES = [
    "Baseline: Shared Cuisine Hierarchy", "Political Ideology", "Forms of Capital",
    "Behavioral Practices and Taste Dispositions", "Data Source",
    "Survey Operationalization and Variable Measurement",
    "Modeling Strategy: Bayesian Adjacent Category Models",
    "Bayesian Model Fit and Taxonomy Comparison",
    "Baseline Authenticity by Cuisine: Cross-Specification Consensus",
    "Cross-Specification Consensus and Fixed Effects Stability Envelope",
    "Substantive Mechanisms & Theoretical Hypotheses",
    "Category-Specific Midpoint Contrast Analyses",
    "Cuisine-Specific Heterogeneity and Consecration Hierarchies"
]

H3_TITLES = [
    "Cuisine Authenticity", "Cultural Capital and Aesthetic Socialization",
    "Behavioral Dining and Cultural Practices", "Food Taste Dispositions",
    "Cosmopolitan Capital", "Socio-Demographic Variables",
    "Political Ideology: Differentiation and Asymmetry (Hypotheses 2 & 3)",
    "Cultural Capital Dual Mechanism: Institutional Distinction vs. Embodied Socialization (Hypotheses 4 & 6)",
    "Behavioral Dining Practices and Active Cultural Consumption (Hypothesis 7)",
    "Bourdieu Taste Dispositions and Authenticity Construct Validation (Hypothesis 8)",
    "Cosmopolitan Capital and Bridging Social Networks (Hypothesis 5)",
    "Sociodemographic and Ethnoracial Anchors",
    "Political Ideology Midpoint Contrasts",
    "Cultural Capital Midpoint Contrasts",
    "Behavioral Dining Practices Midpoint Contrasts",
    "Bourdieu Taste Dispositions Midpoint Contrasts",
    "Cosmopolitan Capital Midpoint Contrasts",
    "Domain 1: Political Ideology Slopes Across Cuisines",
    "Domain 1: Cultural Capital Slopes Across Cuisines",
    "Domain 2: Behavioral Dining Practices Slopes Across Cuisines",
    "Domain 3: Bourdieu Taste Dispositions Slopes Across Cuisines",
    "Domain 4: Cosmopolitan Capital and Social Networks Slopes Across Cuisines"
]

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

def split_markdown_row(line):
    """Splits a markdown table row by pipes while respecting code backticks."""
    line = line.strip()
    if line.startswith("|"):
        line = line[1:]
    if line.endswith("|"):
        line = line[:-1]
        
    cells = []
    cur = []
    in_backtick = False
    for ch in line:
        if ch == '`':
            in_backtick = not in_backtick
            cur.append(ch)
        elif ch == '|' and not in_backtick:
            cells.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    cells.append("".join(cur).strip())
    return cells

def parse_markdown_table(file_path):
    if not os.path.exists(file_path):
        return [], [], []
    with open(file_path, "r", encoding="utf-8") as f:
        lines = [line.strip() for line in f if line.strip()]
    table_lines = [line for line in lines if line.startswith("|") and line.endswith("|")]
    if len(table_lines) < 3:
        return [], [], []
    
    headers = split_markdown_row(table_lines[0])
    align_defs = split_markdown_row(table_lines[1])
    alignments = []
    for ad in align_defs:
        if ad.startswith(":") and ad.endswith(":"):
            alignments.append("center")
        elif ad.endswith(":"):
            alignments.append("right")
        else:
            alignments.append("left")
            
    rows = []
    for line in table_lines[2:]:
        rows.append(split_markdown_row(line))
    return headers, rows, alignments

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

def create_apa_table_xml(headers, rows_data, col_widths=None, alignments=None):
    total_w = 9360  # 6.5 inches portrait width in dxa
    num_cols = len(headers)
    
    if col_widths is None:
        col1_w = int(total_w * 0.36)
        rem_w = total_w - col1_w
        sub_w = int(rem_w / (num_cols - 1))
        col_widths = [col1_w] + [sub_w] * (num_cols - 2)
        col_widths.append(total_w - sum(col_widths))
        
    if alignments is None:
        alignments = ["left" if i == 0 else "center" for i in range(num_cols)]
    
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
        align = alignments[i] if i < len(alignments) else "center"
        clean_h = h.replace("$p_{\\text{WAIC}}$", "p_WAIC").replace("$\\Delta \\text{WAIC}_{\\text{vs. Baseline}}$", "ΔWAIC").replace("$\\Delta \\text{WAIC}$", "ΔWAIC").replace("Models ($k$)", "Models (k)")
        clean_plain_h = re.sub(r'[*`_]', '', clean_h).strip()
        nowrap_elem = '<w:noWrap/>' if (len(clean_plain_h) <= 10 and ' ' not in clean_plain_h) else ''
        xml.append(
            f'<w:tc><w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/>'
            f'<w:tcBorders><w:bottom w:val="single" w:sz="4" w:space="0" w:color="000000"/></w:tcBorders>{nowrap_elem}</w:tcPr>'
            f'<w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/>'
            f'<w:ind w:left="0" w:right="0" w:firstLine="0"/><w:jc w:val="{align}"/></w:pPr>'
            f'<w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">{xml_escape(clean_h)}</w:t></w:r></w:p></w:tc>'
        )
    xml.append('</w:tr>')
    
    # Data Rows
    for row in rows_data:
        xml.append('<w:tr><w:trPr><w:cantSplit/></w:trPr>')
        for i, val in enumerate(row):
            align = alignments[i] if i < len(alignments) else "center"
            runs_xml = format_cell_runs(val)
            w_col = col_widths[i] if i < len(col_widths) else col_widths[-1]
            clean_plain_text = re.sub(r'[*`_]', '', val).strip()
            nowrap_elem = '<w:noWrap/>' if ((len(clean_plain_text) <= 15 and ' ' not in clean_plain_text) or (align == 'center' and len(clean_plain_text) <= 10)) else ''
            xml.append(
                f'<w:tc><w:tcPr><w:tcW w:w="{w_col}" w:type="dxa"/>{nowrap_elem}</w:tcPr>'
                f'<w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/>'
                f'<w:ind w:left="0" w:right="0" w:firstLine="0"/><w:jc w:val="{align}"/></w:pPr>'
                f'{runs_xml}</w:p></w:tc>'
            )
        xml.append('</w:tr>')
        
    xml.append('</w:tbl>')
    return "".join(xml)

def make_p_elem(text, bold=False, italic=False, heading_level=None, align=None, before=0, after=120, first_line_indent=None):
    """Creates a formatted OpenXML paragraph ElementTree element strictly adhering to ECMA-376 and AGENTS guidelines."""
    p = ET.Element(f'{{{W_NS}}}p')
    pPr = ET.SubElement(p, f'{{{W_NS}}}pPr')
    if heading_level:
        pStyle = ET.SubElement(pPr, f'{{{W_NS}}}pStyle')
        pStyle.set(f'{{{W_NS}}}val', f'Heading{heading_level}')
        
    # ECMA-376 tag order: suppressAutoHyphens -> spacing -> ind -> jc
    ET.SubElement(pPr, f'{{{W_NS}}}suppressAutoHyphens')
    
    sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
    sp.set(f'{{{W_NS}}}before', str(before))
    sp.set(f'{{{W_NS}}}after', str(after))
    
    # 720 dxa = 0.5 inches first line indent for normal body paragraphs
    if first_line_indent is None:
        if heading_level is not None or align is not None or bold or text.startswith("Table ") or text.startswith("Figure ") or text.startswith("Note:") or text in H1_TITLES:
            first_line = 0
        else:
            first_line = 720
    else:
        first_line = first_line_indent
        
    ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
    ind.set(f'{{{W_NS}}}left', '0')
    ind.set(f'{{{W_NS}}}right', '0')
    ind.set(f'{{{W_NS}}}firstLine', str(first_line))
    
    if align:
        jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
        jc.set(f'{{{W_NS}}}val', align)
    elif first_line == 720:
        jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
        jc.set(f'{{{W_NS}}}val', 'both')
        
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

def make_equation_1_elem():
    """Builds OpenXML paragraph with OMML math for adjacent-category model Equation (1)."""
    xml = f'''<w:p xmlns:w="{W_NS}" xmlns:m="{M_NS}">
  <w:pPr>
    <w:spacing w:before="160" w:after="160"/>
    <w:ind w:left="0" w:right="0" w:firstLine="0"/>
    <w:jc w:val="center"/>
  </w:pPr>
  <m:oMath>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t>log</m:t></m:r>
    <m:d>
      <m:dPr><m:begChr m:val="["/><m:endChr m:val="]"/></m:dPr>
      <m:e>
        <m:f>
          <m:fPr><m:type m:val="bar"/></m:fPr>
          <m:num>
            <m:r><m:t>P</m:t></m:r>
            <m:d>
              <m:dPr><m:begChr m:val="("/><m:endChr m:val=")"/></m:dPr>
              <m:e>
                <m:sSub>
                  <m:e><m:r><m:t>Y</m:t></m:r></m:e>
                  <m:sub><m:r><m:t>ij</m:t></m:r></m:sub>
                </m:sSub>
                <m:r><m:t> = k + 1</m:t></m:r>
              </m:e>
            </m:d>
          </m:num>
          <m:den>
            <m:r><m:t>P</m:t></m:r>
            <m:d>
              <m:dPr><m:begChr m:val="("/><m:endChr m:val=")"/></m:dPr>
              <m:e>
                <m:sSub>
                  <m:e><m:r><m:t>Y</m:t></m:r></m:e>
                  <m:sub><m:r><m:t>ij</m:t></m:r></m:sub>
                </m:sSub>
                <m:r><m:t> = k</m:t></m:r>
              </m:e>
            </m:d>
          </m:den>
        </m:f>
      </m:e>
    </m:d>
    <m:r><m:t> = </m:t></m:r>
    <m:sSub>
      <m:e><m:r><m:t>η</m:t></m:r></m:e>
      <m:sub><m:r><m:t>ij</m:t></m:r></m:sub>
    </m:sSub>
    <m:r><m:t> − </m:t></m:r>
    <m:sSub>
      <m:e><m:r><m:t>τ</m:t></m:r></m:e>
      <m:sub><m:r><m:t>k</m:t></m:r></m:sub>
    </m:sSub>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t xml:space="preserve">       (1)</m:t></m:r>
  </m:oMath>
</w:p>'''
    return ET.fromstring(xml)

def make_equation_2_elem():
    """Builds OpenXML paragraph with OMML math for marginal transition-averaged parameter."""
    xml = f'''<w:p xmlns:w="{W_NS}" xmlns:m="{M_NS}">
  <w:pPr>
    <w:spacing w:before="160" w:after="160"/>
    <w:ind w:left="0" w:right="0" w:firstLine="0"/>
    <w:jc w:val="center"/>
  </w:pPr>
  <m:oMath>
    <m:acc>
      <m:accPr><m:chr m:val="&#772;"/></m:accPr>
      <m:e>
        <m:sSub>
          <m:e><m:r><m:t>β</m:t></m:r></m:e>
          <m:sub><m:r><m:t>j</m:t></m:r></m:sub>
        </m:sSub>
      </m:e>
    </m:acc>
    <m:r><m:t> = </m:t></m:r>
    <m:f>
      <m:fPr><m:type m:val="bar"/></m:fPr>
      <m:num><m:r><m:t>1</m:t></m:r></m:num>
      <m:den><m:r><m:t>6</m:t></m:r></m:den>
    </m:f>
    <m:nary>
      <m:naryPr><m:chr m:val="&#8721;"/><m:limLoc m:val="undOvr"/></m:naryPr>
      <m:sub><m:r><m:t>k=1</m:t></m:r></m:sub>
      <m:sup><m:r><m:t>6</m:t></m:r></m:sup>
      <m:e>
        <m:sSubSup>
          <m:e><m:r><m:t>β</m:t></m:r></m:e>
          <m:sub><m:r><m:t>j</m:t></m:r></m:sub>
          <m:sup><m:r><m:t>(k)</m:t></m:r></m:sup>
        </m:sSubSup>
      </m:e>
    </m:nary>
  </m:oMath>
</w:p>'''
    return ET.fromstring(xml)

def make_equation_3_elem():
    """Builds OpenXML paragraph with OMML math for midpoint contrast log-odds."""
    xml = f'''<w:p xmlns:w="{W_NS}" xmlns:m="{M_NS}">
  <w:pPr>
    <w:spacing w:before="160" w:after="160"/>
    <w:ind w:left="0" w:right="0" w:firstLine="0"/>
    <w:jc w:val="center"/>
  </w:pPr>
  <m:oMath>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t>ΔLog-Odds(</m:t></m:r>
    <m:r><m:t>k</m:t></m:r>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t> vs. 4) = </m:t></m:r>
    <m:nary>
      <m:naryPr><m:chr m:val="&#8721;"/><m:limLoc m:val="undOvr"/></m:naryPr>
      <m:sub><m:r><m:t>m=4</m:t></m:r></m:sub>
      <m:sup><m:r><m:t>k−1</m:t></m:r></m:sup>
      <m:e>
        <m:sSup>
          <m:e><m:r><m:t>β</m:t></m:r></m:e>
          <m:sup><m:r><m:t>(m)</m:t></m:r></m:sup>
        </m:sSup>
      </m:e>
    </m:nary>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t xml:space="preserve">   (for k &gt; 4),    −</m:t></m:r>
    <m:nary>
      <m:naryPr><m:chr m:val="&#8721;"/><m:limLoc m:val="undOvr"/></m:naryPr>
      <m:sub><m:r><m:t>m=k</m:t></m:r></m:sub>
      <m:sup><m:r><m:t>3</m:t></m:r></m:sup>
      <m:e>
        <m:sSup>
          <m:e><m:r><m:t>β</m:t></m:r></m:e>
          <m:sup><m:r><m:t>(m)</m:t></m:r></m:sup>
        </m:sSup>
      </m:e>
    </m:nary>
    <m:r><m:rPr><m:sty m:val="p"/></m:rPr><m:t xml:space="preserve">   (for k &lt; 4)</m:t></m:r>
  </m:oMath>
</w:p>'''
    return ET.fromstring(xml)

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
                    rPr.remove(rFonts)

def get_p_text(elem):
    runs_text = []
    for t in elem.iter():
        if t.tag in [f'{{{W_NS}}}t', f'{{{M_NS}}}t']:
            if t.text:
                runs_text.append(t.text)
    return "".join(runs_text).strip()

def normalize_document_paragraphs(body):
    """
    Standard Paragraph Normalization Pattern (Section 8 of global AGENTS.md).
    Normalizes headings, table/figure captions, notes, drawings, and body prose.
    """
    body_elements = list(body)
    to_remove = []
    
    for elem in body_elements:
        if not elem.tag.endswith('p'):
            continue
        text = get_p_text(elem)
        drawings = elem.findall(f'.//{{{W_NS}}}drawing')
        
        # 1. Purge empty heading paragraphs
        if not text and not drawings:
            pPr = elem.find(f'{{{W_NS}}}pPr')
            pStyle = pPr.find(f'{{{W_NS}}}pStyle') if pPr is not None else None
            if pStyle is not None and pStyle.get(f'{{{W_NS}}}val') in ['Heading1', 'Heading2', 'Heading3', 'Title', 'Subtitle']:
                to_remove.append(elem)
            continue
            
        pPr = elem.find(f'{{{W_NS}}}pPr')
        if pPr is None:
            pPr = ET.SubElement(elem, f'{{{W_NS}}}pPr')
            
        def reset_pPr(pPr_elem):
            for child in list(pPr_elem):
                pPr_elem.remove(child)
                
        def set_ind(pPr_elem, left=0, right=0, first_line=0):
            ind = pPr_elem.find(f'{{{W_NS}}}ind')
            if ind is None:
                ind = ET.SubElement(pPr_elem, f'{{{W_NS}}}ind')
            ind.set(f'{{{W_NS}}}left', str(left))
            ind.set(f'{{{W_NS}}}right', str(right))
            ind.set(f'{{{W_NS}}}firstLine', str(first_line))
            # Explicitly remove hanging if present
            if 'hanging' in ind.attrib:
                del ind.attrib['hanging']

        # 2. Level 1, 2, 3 Headings
        if text in H1_TITLES or text in H2_TITLES or text in H3_TITLES:
            h_style = "Heading1" if text in H1_TITLES else ("Heading2" if text in H2_TITLES else "Heading3")
            reset_pPr(pPr)
            pStyle = ET.SubElement(pPr, f'{{{W_NS}}}pStyle')
            pStyle.set(f'{{{W_NS}}}val', h_style)
            set_ind(pPr, 0, 0, 0)
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'left')
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            if h_style == "Heading1":
                sp.set(f'{{{W_NS}}}before', '300')
                sp.set(f'{{{W_NS}}}after', '80')
            elif h_style == "Heading2":
                sp.set(f'{{{W_NS}}}before', '200')
                sp.set(f'{{{W_NS}}}after', '60')
            else:
                sp.set(f'{{{W_NS}}}before', '120')
                sp.set(f'{{{W_NS}}}after', '40')
            for r in elem.findall(f'.//{{{W_NS}}}r'):
                rPr = r.find(f'{{{W_NS}}}rPr')
                if rPr is not None:
                    for child in list(rPr):
                        if child.tag.split('}')[-1] in ['b', 'bCs', 'color', 'sz', 'szCs', 'rFonts', 'vertAlign', 'shd', 'highlight']:
                            rPr.remove(child)
                            
        # 3. Table Captions (0 indent, bold, left)
        elif re.match(r'^Table\s+\d+\.', text):
            reset_pPr(pPr)
            set_ind(pPr, 0, 0, 0)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '180')
            sp.set(f'{{{W_NS}}}after', '80')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'left')
            for r in elem.findall(f'.//{{{W_NS}}}r'):
                rPr = r.find(f'{{{W_NS}}}rPr') or ET.SubElement(r, f'{{{W_NS}}}rPr')
                for child in list(rPr):
                    if child.tag.split('}')[-1] in ['vertAlign', 'sz', 'szCs', 'color', 'rFonts', 'highlight', 'shd']:
                        rPr.remove(child)
                if rPr.find(f'{{{W_NS}}}b') is None:
                    ET.SubElement(rPr, f'{{{W_NS}}}b')
                
        # 4. Figure Captions (0 indent, bold, justified)
        elif re.match(r'^Figure\s+\d+\.', text):
            reset_pPr(pPr)
            set_ind(pPr, 0, 0, 0)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '180')
            sp.set(f'{{{W_NS}}}after', '80')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'both')
            for r in elem.findall(f'.//{{{W_NS}}}r'):
                rPr = r.find(f'{{{W_NS}}}rPr') or ET.SubElement(r, f'{{{W_NS}}}rPr')
                for child in list(rPr):
                    if child.tag.split('}')[-1] in ['vertAlign', 'sz', 'szCs', 'color', 'rFonts', 'highlight', 'shd']:
                        rPr.remove(child)
                if rPr.find(f'{{{W_NS}}}b') is None:
                    ET.SubElement(rPr, f'{{{W_NS}}}b')
                
        # 5. Table & Figure Notes (0 indent, Normal style, NOT Heading4)
        elif text.startswith("Note:") or text.startswith("*Note:"):
            reset_pPr(pPr)
            pStyle = ET.SubElement(pPr, f'{{{W_NS}}}pStyle')
            pStyle.set(f'{{{W_NS}}}val', 'Normal')
            set_ind(pPr, 0, 0, 0)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '80')
            sp.set(f'{{{W_NS}}}after', '160')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'both')
            for r in elem.findall(f'.//{{{W_NS}}}r'):
                rPr = r.find(f'{{{W_NS}}}rPr')
                if rPr is not None:
                    for child in list(rPr):
                        if child.tag.split('}')[-1] in ['vertAlign', 'sz', 'szCs', 'color', 'rFonts', 'highlight', 'shd']:
                            rPr.remove(child)
                            
        # 6. Figure Drawings (0 indent, center)
        elif drawings:
            reset_pPr(pPr)
            set_ind(pPr, 0, 0, 0)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '120')
            sp.set(f'{{{W_NS}}}after', '60')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'center')
            
        # 7. Math equations / Centered formulas (standalone equation paragraphs)
        elif (elem.find(f'.//{{{M_NS}}}oMath') is not None and len(text) < 120) or any(text.startswith(k) for k in ["β̄_j =", "β̄j =", "ΔLog-Odds", "log [", "log[", "logk[", "logP"]):
            reset_pPr(pPr)
            set_ind(pPr, 0, 0, 0)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '140')
            sp.set(f'{{{W_NS}}}after', '140')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'center')
            
        # 8. Standard Body Prose (Inherits Normal style 0.5" firstLine indent, justified)
        else:
            reset_pPr(pPr)
            pStyle = ET.SubElement(pPr, f'{{{W_NS}}}pStyle')
            pStyle.set(f'{{{W_NS}}}val', 'Normal')
            set_ind(pPr, 0, 0, 720)
            sp = ET.SubElement(pPr, f'{{{W_NS}}}spacing')
            sp.set(f'{{{W_NS}}}before', '0')
            sp.set(f'{{{W_NS}}}after', '120')
            jc = ET.SubElement(pPr, f'{{{W_NS}}}jc')
            jc.set(f'{{{W_NS}}}val', 'both')
            runs = elem.findall(f'.//{{{W_NS}}}r')
            is_full_bold = len(text) > 80 and all(r.find(f'.//{{{W_NS}}}b') is not None for r in runs if "".join(r.itertext()).strip())
            for r in runs:
                rPr = r.find(f'{{{W_NS}}}rPr')
                if rPr is not None:
                    if is_full_bold:
                        for b_tag in ['b', 'bCs']:
                            b_el = rPr.find(f'{{{W_NS}}}{b_tag}')
                            if b_el is not None:
                                rPr.remove(b_el)
                    for child in list(rPr):
                        if child.tag.split('}')[-1] in ['vertAlign', 'sz', 'szCs', 'color', 'rFonts', 'highlight', 'shd']:
                            rPr.remove(child)
                            
    for elem in to_remove:
        if elem in body:
            body.remove(elem)

def cleanup_scratch_files(keep=None):
    """Purges transient scratch files matching global AGENTS guidelines."""
    keep_set = set(os.path.abspath(f) for f in (keep or []) if os.path.exists(f))
    patterns = ["draft_*.docx", "draft_*.txt", "replacements.json", "*.tmp"]
    for pat in patterns:
        for f in glob.glob(pat):
            if os.path.abspath(f) not in keep_set:
                try:
                    os.remove(f)
                except OSError:
                    pass

def sync_docx(in_docx, out_docx):
    print(f"Reading input document: {in_docx}")
    with zipfile.ZipFile(in_docx, "r") as zin:
        xml_bytes = zin.read("word/document.xml")
        rels_bytes = zin.read("word/_rels/document.xml.rels")
        styles_bytes = zin.read("word/styles.xml")
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}
    
    # 1. Clean styles.xml to strictly Alegreya Sans and configure Normal style indentation (720 dxa = 0.5 in)
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
                ind.set(f'{{{W_NS}}}firstLine', '720')
                if 'hanging' in ind.attrib:
                    del ind.attrib['hanging']

    for style in styles_tree.iter(f'{{{W_NS}}}style'):
        s_id = style.get(f'{{{W_NS}}}styleId', '')
        pPr = style.find(f'{{{W_NS}}}pPr')
        if pPr is None:
            pPr = ET.SubElement(style, f'{{{W_NS}}}pPr')
        ind = pPr.find(f'{{{W_NS}}}ind')
        if ind is None:
            ind = ET.SubElement(pPr, f'{{{W_NS}}}ind')
        ind.set(f'{{{W_NS}}}left', '0')
        ind.set(f'{{{W_NS}}}right', '0')
        if 'hanging' in ind.attrib:
            del ind.attrib['hanging']
        if s_id == "Normal":
            ind.set(f'{{{W_NS}}}firstLine', '720')
        else:
            ind.set(f'{{{W_NS}}}firstLine', '0')
            
    all_files["word/styles.xml"] = ET.tostring(styles_tree, encoding="utf-8", xml_declaration=True)
    print("Sanitized word/styles.xml to Alegreya Sans and set Normal style first-line indent to 0.5 in (720 dxa).")

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
    
    # 2. Prepare relationships for all 12 Figures
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
        (1, "Plots/cuisine_random_effects.png", "image1.png",
         "Figure 1. Baseline Cuisine Authenticity Hierarchy: Cross-Specification Consensus.",
         "Note: Cuisine-specific random intercepts estimated across baseline specifications, capturing the underlying domestic elder versus professional chef baseline ordering."),
        (2, "Plots/fixed_effects_stability_forest.png", "image2.png",
         "Figure 2. Cross-Specification Parameter Stability Envelope Across All 18 Models.",
         "Note: Half-eye posterior distributions synthesize draws across all eighteen models, with point medians, 80% and 95% credible intervals, and individual model median tick marks (+)."),
        (3, "Plots/ideology_cs_midpoint_effects.png", "image3.png",
         "Figure 3. Political Ideology Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Social conservatism exhibits an escalating positive contrast trajectory across the upper scale, reaching its highest contrast at Category 7 (Professional Chef: median ≈ +0.45). In contrast, economic conservatism exerts a moderating influence, concentrating ratings around the neutral midpoint."),
        (4, "Plots/cultural_cs_midpoint_effects.png", "image4.png",
         "Figure 4. Cultural Capital Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Educational attainment systematically increases the likelihood of rating cuisines toward the professional chef anchor (Categories 5–7). Parental education dampens extreme categories, while childhood arts exposure shows diffuse intervals spanning zero when adult credentials are held constant."),
        (5, "Plots/practices_cs_midpoint_effects.png", "image5.png",
         "Figure 5. Dining Practices Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Adult highbrow arts participation suppresses ratings for domestic elder categories while elevating upper restaurant categories. Fine dining frequency exhibits an escalating monotonic trajectory across the upper scale, peaking at Category 7 (Chef vs. 4: median = +0.69, 95% CrI [0.45, 0.95], P > 0 = 100%)."),
        (6, "Plots/dispositions_cs_midpoint_effects.png", "image6.png",
         "Figure 6. Bourdieu Taste Dispositions Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Respondents prioritizing exotic and authentic flavors exhibit an elevation across all domestic elder categories relative to the midpoint (Category 1: median = +0.39, 95% CrI [0.20, 0.59]), alongside depression of the extreme chef category (Category 7: -0.15)."),
        (7, "Plots/cosmopolitan_cs_midpoint_effects.png", "image7.png",
         "Figure 7. Cosmopolitan Capital Multi-Model Consensus Category-Specific Midpoint Contrasts (Relative to Category 4: Neutral Midpoint).",
         "Note: Friendship network diversity credibly increases the log-odds of selecting professional chef categories relative to the neutral midpoint (Category 5: +0.21; Category 6: +0.28; Category 7: +0.31), while depressing domestic elder categories."),
        (8, "Plots/rs_cuisine_slopes_ideology.png", "image8.png",
         "Figure 8. Cuisine Random Slopes: Ideology (Social vs. Economic Conservatism), Partitioned by Consecration Tiers.",
         "Note: Social conservatism acts as a countervailing force, generating positive slopes across subaltern cuisines (Native American median = +0.30, Pakistani median = +0.22, Lebanese median = +0.21, Jamaican median = +0.21, Mexican median = +0.21, Nigerian median = +0.18) that pull peripheral traditions toward the professional chef boundary."),
        (9, "Plots/rs_cuisine_slopes_cultural.png", "image9.png",
         "Figure 9. Cuisine Random Slopes: Cultural Capital (Education, Parental Education, Childhood Arts), Partitioned by Consecration Tiers.",
         "Note: Respondent education demonstrates positive slopes across cuisines, with traditions such as Native American (median = +0.19), Italian (+0.15), Mexican (+0.15), and Swedish (+0.14) showing positive shifts toward chef craftsmanship."),
        (10, "Plots/rs_cuisine_slopes_practices.png", "image10.png",
         "Figure 10. Cuisine Random Slopes: Dining Practices (Highbrow Arts, Fine Dining, Fast Food), Partitioned by Consecration Tiers.",
         "Note: Adult highbrow arts attendance and fine dining frequency exert positive pro-chef pulls across both haute and subaltern culinary traditions—most notably on Native American (median = +0.20), Mexican (median = +0.18), and Italian cuisines (median = +0.16)."),
        (11, "Plots/rs_cuisine_slopes_dispositions.png", "image11.png",
         "Figure 11. Cuisine Random Slopes: Taste Dispositions (Exotic & Authentic vs. Conventional & Familiar), Partitioned by Consecration Tiers.",
         "Note: Liking 'exotic and authentic' food credibly reinforces the domestic elder orientation across nearly all subaltern and non-Western cuisines—including Nigerian (-0.13), Peruvian (-0.13), Moroccan (-0.12), Ethiopian (-0.12), Pakistani (-0.11), Jamaican (-0.10), Lebanese (-0.10), and Korean (-0.10)."),
        (12, "Plots/rs_cuisine_slopes_cosmopolitan.png", "image12.png",
         "Figure 12. Cuisine Random Slopes: Cosmopolitan Capital (Global Citizen Identity, Friendship Network Diversity), Partitioned by Consecration Tiers.",
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
    
    figures_map = {
        f_num: (f_path, f_target_name, caption_txt, note_txt)
        for f_num, f_path, f_target_name, caption_txt, note_txt in figures_data
    }
    
    def append_figure_elem(elements_list, f_num):
        f_path, _, caption_txt, note_txt = figures_map[f_num]
        rid = fig_rids[f_num]
        elements_list.append(make_drawing_elem(rid, f_path, 100 + f_num))
        elements_list.append(make_p_elem(caption_txt, bold=True, before=80, after=60))
        elements_list.append(make_p_elem(note_txt, italic=True, before=40, after=200))
        
    def append_table_1_elem(elements_list):
        h1, r1, a1 = parse_markdown_table("cache/table_hypotheses.md")
        elements_list.append(make_p_elem("Table 1. Summary of Theoretical Hypotheses.", bold=True, before=180, after=80))
        if h1 and r1:
            tbl1_xml = create_apa_table_xml(h1, r1, [1000, 2800, 5560], a1)
            elements_list.append(ET.fromstring(tbl1_xml))
            
    def append_table_2_elem(elements_list):
        h2, r2, a2 = parse_markdown_table("cache/table_fit_comparison.md")
        elements_list.append(make_p_elem("Table 2. Model Comparison and Predictive Fit Progression for Nested Bayesian Mixed-Effects Models (M1–M18).", bold=True, before=180, after=80))
        if h2 and r2:
            tbl2_xml = create_apa_table_xml(h2, r2, [1000, 5160, 1600, 1600], a2)
            elements_list.append(ET.fromstring(tbl2_xml))
        elements_list.append(make_p_elem(
            "Note: N = 18,180 ratings across 1,212 respondents evaluating 15 cuisines. WAIC denotes the Widely Applicable Information Criterion; lower values indicate superior expected out-of-sample predictive accuracy. ΔWAIC is calculated relative to the baseline reference model (M1: Base Strict RI).",
            italic=True, before=80, after=160
        ))

    # 3. Parse and Clean Body Text (Non-destructive of user edits on Google Drive)
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
    
    heading_to_item = {
        "Bayesian Model Fit and Taxonomy Comparison": ("table", 2),
        "Baseline Authenticity by Cuisine: Cross-Specification Consensus": ("fig", 1),
        "Cross-Specification Consensus and Fixed Effects Stability Envelope": ("fig", 2),
        "Political Ideology Midpoint Contrasts": ("fig", 3),
        "Cultural Capital Midpoint Contrasts": ("fig", 4),
        "Behavioral Dining Practices Midpoint Contrasts": ("fig", 5),
        "Bourdieu Taste Dispositions Midpoint Contrasts": ("fig", 6),
        "Cosmopolitan Capital Midpoint Contrasts": ("fig", 7),
        "Domain 1: Political Ideology Slopes Across Cuisines": ("fig", 8),
        "Domain 1: Cultural Capital Slopes Across Cuisines": ("fig", 9),
        "Domain 2: Behavioral Dining Practices Slopes Across Cuisines": ("fig", 10),
        "Domain 3: Bourdieu Taste Dispositions Slopes Across Cuisines": ("fig", 11),
        "Domain 4: Cosmopolitan Capital and Social Networks Slopes Across Cuisines": ("fig", 12),
    }
    
    inserted_items = set()
    active_item = None
    
    def is_project_table_elem(el):
        t = el.tag.split('}')[-1]
        text_content = ''.join(el.itertext()).strip()
        if t == 'tbl':
            if any(h in text_content for h in ["Hypothesis Name", "Summary of Theoretical", "Specification / Architecture", "Base Strict RI", "Base Relaxed CS", "Omnibus Meta", "Cuisine Consecration Hierarchy"]):
                return True
        return False

    def is_project_table_or_fig_caption_or_note(el):
        text_content = ''.join(el.itertext()).strip()
        if re.match(r"^Table\s+\d+\.", text_content) or re.match(r"^Table\s+\d+:", text_content):
            return True
        if re.match(r"^Figure\s+\d+\.", text_content) or re.match(r"^Figure\s+\d+:", text_content):
            return True
        if (text_content.startswith("Note:") or text_content.startswith("*Note:")) and any(k in text_content for k in [
            "WAIC denotes", "N = 18,180", "Half-eye posterior", "Cuisine-specific random",
            "Social conservatism exhibits", "Educational attainment systematically",
            "Adult highbrow arts", "Respondents prioritizing exotic", "Friendship network diversity",
            "Social conservatism acts as", "Respondent education demonstrates",
            "Adult highbrow arts attendance", "Liking 'exotic and authentic'",
            "Baseline reference model", "Summary of cuisine-specific", "Marginal transition-averaged"
        ]):
            return True
        return False

    for elem in orig_elements:
        tag = elem.tag.split('}')[-1]
        txt = ''.join(elem.itertext()).strip()
        
        # Stop before legacy TABLES/FIGURES section if one exists at the end of the document
        if txt in ["TABLES", "FIGURES"]:
            break
            
        # Cleanly skip any previously injected project tables, figure drawings, captions, or notes
        if is_project_table_elem(elem) or is_project_table_or_fig_caption_or_note(elem):
            continue
        if elem.findall(f'.//{{{W_NS}}}drawing') and not txt:
            continue
            
        # Table 1 Anchor: placed at the conclusion of HYPOTHESES (directly before DATA AND METHODS)
        if txt in ["DATA AND METHODS", "4. DATA AND METHODS", "Data and Methods", "Data and methods"] and "table_1" not in inserted_items:
            append_table_1_elem(clean_body_elements)
            inserted_items.add("table_1")
            
        # If transitioning to a new heading, flush any pending inline figure/table for the preceding section
        if (txt in H1_TITLES or txt in H2_TITLES or txt in H3_TITLES) and active_item and active_item not in inserted_items:
            item_type, item_num = active_item
            if item_type == "table" and item_num == 2:
                append_table_2_elem(clean_body_elements)
            elif item_type == "fig":
                append_figure_elem(clean_body_elements, item_num)
            inserted_items.add(f"{item_type}_{item_num}")
            active_item = None
            
        # Equation replacements
        if "logk[" in txt or ("P(Yij" in txt and "ηij" in txt):
            clean_body_elements.append(make_equation_1_elem())
            continue
        elif "β̄j =" in txt or "β̄_j =" in txt or ("(⅙) ∑" in txt):
            clean_body_elements.append(make_equation_2_elem())
            continue
        elif "ΔLog-Odds(k vs. 4)" in txt or "ΔLog-Odds" in txt:
            clean_body_elements.append(make_equation_3_elem())
            continue

        # Preserve user paragraph / element exactly as in live document
        clean_body_elements.append(elem)
        
        # Check if this heading introduces a section requiring an inline figure or table
        for h_title, item_spec in heading_to_item.items():
            if txt == h_title or txt.startswith(h_title):
                active_item = item_spec
                
    # Flush any trailing active item (e.g. Figure 12 at the end of Domain 4)
    if active_item and f"{active_item[0]}_{active_item[1]}" not in inserted_items:
        item_type, item_num = active_item
        if item_type == "table" and item_num == 2:
            append_table_2_elem(clean_body_elements)
        elif item_type == "fig":
            append_figure_elem(clean_body_elements, item_num)
        inserted_items.add(f"{item_type}_{item_num}")
        
    # Re-attach sectPr
    if sectPr is not None:
        clean_body_elements.append(sectPr)
        
    # 4. Rebuild body with clean elements
    body.clear()
    for elem in clean_body_elements:
        body.append(elem)
        
    # Apply standard document paragraph normalization (Section 8 of AGENTS.md)
    normalize_document_paragraphs(body)
        
    # Re-attach sectPr
    if sectPr is not None:
        clean_body_elements.append(sectPr)
        
    # 6. Rebuild body with clean elements
    body.clear()
    for elem in clean_body_elements:
        body.append(elem)
        
    # Apply standard document paragraph normalization (Section 8 of AGENTS.md)
    normalize_document_paragraphs(body)
    
    # Sanitize all fonts across the document tree
    sanitize_tree_fonts(doc_tree)
    
    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)
    
    # Also sanitize footer
    if "word/footer1.xml" in all_files:
        footer_tree = ET.fromstring(all_files["word/footer1.xml"])
        sanitize_tree_fonts(footer_tree)
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
    if "--cleanup" in sys.argv:
        cleanup_scratch_files(keep=[out_doc])
