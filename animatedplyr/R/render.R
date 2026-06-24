# =============================================================================
# R/render.R — payload -> self-contained HTML/CSS/JS
#
# Template strategy: tokens like __ID__ are replaced with gsub(fixed = TRUE).
# This avoids sprintf format-string fragility entirely. CSS percent signs are
# safe because we never pass the template through sprintf.
# =============================================================================

.TEMPLATE <- '
<style>
#__ID__ {
  --box: __BOX__px;
  --font-size: __FONT_SIZE__px;
  --font-weight: __FONT_WEIGHT__;
  --transition: __TRANSITION__ms;
  --canvas-bg: __CANVAS_BG__;
  --canvas-pad: __CANVAS_PAD__px;
  --title-bg: __TITLE_BG__;
  --title-color: __TITLE_COLOR__;
  --cell-bg: __CELL_BG__;
  --cell-color: __CELL_COLOR__;
  --header-color: __HEADER_COLOR__;
  --border-color: __BORDER_COLOR__;
  --border-width: __BORDER_WIDTH__px;
  --border-radius: __BORDER_RADIUS__px;
  --gap: __GAP__px;
  --cell-opacity: __CELL_OPACITY__;
  --keep-color: __KEEP_COLOR__;
  --drop-color: __DROP_COLOR__;
  --new-color: __NEW_COLOR__;
  font-family: "__FONT_FAMILY__", monospace;
  background: var(--canvas-bg); color: #222;
  padding: var(--canvas-pad); border: 1px solid #eee; border-radius: 6px;
  display: flex; flex-direction: column; align-items: center; gap: 1.1rem;
}
#__ID__ * { box-sizing: border-box; }
#__ID__ .anim-title {
  font-family: "__FONT_FAMILY__", monospace;
  font-size: calc(var(--font-size) + 5px); font-weight: 700;
  background: var(--title-bg); color: var(--title-color);
  padding: 7px 18px; border-radius: 4px;
  display: inline-block; margin: 0;
}
#__ID__ .subtitle { font-size: calc(var(--font-size) - 1px); color: #888; margin-top: 6px; }
#__ID__ .title-block { text-align: center; }
#__ID__ .grid { display: flex; flex-direction: column; gap: var(--gap); }
#__ID__ .grid-row {
  display: flex; gap: var(--gap); justify-content: center;
  transition: opacity var(--transition) ease, max-height var(--transition) ease;
  overflow: hidden; max-height: 64px;
}
#__ID__ .grid-row.row-faded { opacity: 0; max-height: 0; pointer-events: none; }
#__ID__ .cell {
  width: var(--box); height: calc(var(--box) * 0.42);
  display: flex; align-items: center; justify-content: center;
  font-size: var(--font-size); font-weight: var(--font-weight);
  opacity: var(--cell-opacity);
  border-radius: var(--border-radius);
  border: var(--border-width) solid var(--border-color);
  transition: opacity var(--transition) ease, background-color var(--transition) ease,
              color var(--transition) ease, box-shadow var(--transition) ease,
              transform var(--transition) ease;
  white-space: nowrap; overflow: hidden;
}
#__ID__ .cell.header { color: var(--header-color); font-size: calc(var(--font-size) - 2px); height: calc(var(--box) * 0.36); }
#__ID__ .cell.data { background: var(--cell-bg); color: var(--cell-color); }
#__ID__ .cell.faded { opacity: 0; }
#__ID__ .cell.data.flag-keep { box-shadow: 0 0 0 2px var(--keep-color) inset; background: #E8F5E9; }
#__ID__ .cell.data.flag-drop { box-shadow: 0 0 0 2px var(--drop-color) inset; background: #FFEBEE; color: #B71C1C; }
#__ID__ .cell.data.flag-new  {
  box-shadow: 0 0 0 2px var(--new-color) inset; background: #E3F2FD; color: #0D47A1;
  font-weight: 700; animation: pop___ID__ 500ms ease;
}
#__ID__ .cell.header.flag-new-header { animation: pop___ID__ 500ms ease; }
@keyframes pop___ID__ {
  0%   { transform: scale(0.6); opacity: 0; }
  60%  { transform: scale(1.05); opacity: 1; }
  100% { transform: scale(1); opacity: 1; }
}
#__ID__ .step-label {
  font-size: var(--font-size); color: #555; font-style: italic;
  min-height: 20px; text-align: center; max-width: 560px;
}
#__ID__ .pill {
  font-size: calc(var(--font-size) - 2px); color: #888;
  background: #efefef; border: 1px solid #e0e0e0;
  padding: 3px 12px; border-radius: 12px; display: none;
}
#__ID__ .callout {
  font-size: var(--font-size); color: #555; background: #fff;
  border: 1.5px dashed #bbb; border-radius: 6px;
  padding: 18px 26px; text-align: center; max-width: 480px; display: none;
}
#__ID__ .controls { display: flex; gap: 8px; }
#__ID__ .controls button {
  padding: 6px 18px; border-radius: 4px; border: 1.5px solid #ccc;
  background: #fff; color: #222; cursor: pointer;
  font-family: "__FONT_FAMILY__", monospace;
  font-size: calc(var(--font-size) - 1px); font-weight: 600;
}
#__ID__ .controls button:hover { background: #f0f0f0; border-color: #aaa; }
#__ID__ .controls button:disabled { opacity: 0.35; cursor: default; }
#__ID__ .progress { display: flex; gap: 7px; }
#__ID__ .dot {
  width: 7px; height: 7px; border-radius: 50%;
  background: #ddd; transition: background 0.3s; cursor: pointer;
}
#__ID__ .dot.active { background: #222; }
</style>

<div id="__ID__">
  <div class="title-block">
    <h1 class="anim-title"></h1>
    <p class="subtitle">Step through the animation or press Play</p>
  </div>
  <div class="grid"></div>
  <div class="callout"></div>
  <div class="step-label"></div>
  <div class="pill"></div>
  <div class="progress"></div>
  <div class="controls">
    <button class="btn-prev">&larr; Prev</button>
    <button class="btn-play">&#9654; Play</button>
    <button class="btn-next">Next &rarr;</button>
  </div>
</div>

<script>
(function() {
  var PAYLOAD = __PAYLOAD_JSON__;
  var STATIC_STEP = __STATIC_STEP__;

  var ROOT = document.getElementById("__ID__");
  if (!ROOT) return;

  var titleEl    = ROOT.querySelector(".anim-title");
  var gridEl     = ROOT.querySelector(".grid");
  var calloutEl  = ROOT.querySelector(".callout");
  var labelEl    = ROOT.querySelector(".step-label");
  var pillEl     = ROOT.querySelector(".pill");
  var progressEl = ROOT.querySelector(".progress");
  var btnPrev    = ROOT.querySelector(".btn-prev");
  var btnNext    = ROOT.querySelector(".btn-next");
  var btnPlay    = ROOT.querySelector(".btn-play");
  var controlsEl = ROOT.querySelector(".controls");

  var COLORS  = PAYLOAD.colors || {};
  var BEFORE  = PAYLOAD.before;
  var AFTER   = PAYLOAD.after;
  var CFG     = PAYLOAD.config || {};
  var DUR     = CFG.duration || 1300;

  titleEl.textContent = PAYLOAD.title;

  /* ---------- Disclosure pill ---------- */
  function disclosureText(d) {
    if (!d) return "";
    var parts = [];
    if (d.hidden_rows > 0) parts.push("+" + d.hidden_rows + (d.hidden_rows === 1 ? " row" : " rows"));
    if (d.hidden_cols > 0) parts.push(d.hidden_cols + (d.hidden_cols === 1 ? " col" : " cols"));
    if (!parts.length) return "";
    return parts.join(" \\u00b7 ") + " not shown";
  }
  var pillText = (CFG.show_disclosure !== false) ? disclosureText(PAYLOAD.disclosure) : "";
  if (pillText) { pillEl.textContent = pillText; pillEl.style.display = "inline-block"; }

  /* ---------- Callout short-circuit ---------- */
  if (PAYLOAD.callout) {
    gridEl.style.display = "none";
    progressEl.style.display = "none";
    controlsEl.style.display = "none";
    labelEl.style.display = "none";
    calloutEl.textContent = PAYLOAD.callout;
    calloutEl.style.display = "block";
    return;
  }

  /* ---------- Build STEPS by verb ---------- */
  var STEPS = [];

  if (PAYLOAD.verb === "select") {
    var removed = BEFORE.cols.filter(function(c) { return AFTER.cols.indexOf(c) < 0; });
    STEPS = [
      { label: "Original data",
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows },
      { label: removed.length ? removed.map(function(c){ return c + " fades out..."; }).join(", ")
                              : "All shown columns are selected",
        activeCols: BEFORE.cols, fadedCols: removed, rows: BEFORE.rows },
      { label: "Remaining columns recenter",
        activeCols: AFTER.cols, fadedCols: [], rows: BEFORE.rows },
      { label: "select() complete \\u2014 " + AFTER.cols.length +
               (AFTER.cols.length === 1 ? " column kept" : " columns kept"),
        activeCols: AFTER.cols, fadedCols: [], rows: AFTER.rows }
    ];
  } else if (PAYLOAD.verb === "filter") {
    var flags = PAYLOAD.row_flags || [];
    var dropped = [];
    flags.forEach(function(k, i) { if (!k) dropped.push(i); });
    var kept = flags.filter(Boolean).length;
    var flagMap = {};
    flags.forEach(function(k, i) { flagMap[i] = k ? "keep" : "drop"; });
    STEPS = [
      { label: "Original data",
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows,
        rowFlags: {}, droppedRows: [] },
      { label: "Evaluating condition: " + PAYLOAD.expression,
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows,
        rowFlags: flagMap, droppedRows: [] },
      { label: "Rows that fail the condition fade out...",
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows,
        rowFlags: flagMap, droppedRows: dropped },
      { label: "filter() complete \\u2014 " + kept +
               (kept === 1 ? " row kept" : " rows kept") + " (of those shown)",
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows,
        rowFlags: {}, droppedRows: dropped }
    ];
  } else if (PAYLOAD.verb === "mutate") {
    var newCols = AFTER.cols.filter(function(c) { return BEFORE.cols.indexOf(c) < 0; });
    var src = PAYLOAD.source_col || null;
    STEPS = [
      { label: "Original data",
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows },
      { label: "Computing new column: " + PAYLOAD.expression,
        activeCols: BEFORE.cols, fadedCols: [], rows: BEFORE.rows,
        highlightSource: src },
      { label: newCols.join(", ") + " appears on the right \\u2192",
        activeCols: AFTER.cols, fadedCols: [], rows: AFTER.rows, newCols: newCols },
      { label: "mutate() complete \\u2014 " + newCols.length +
               (newCols.length === 1 ? " new column added" : " new columns added"),
        activeCols: AFTER.cols, fadedCols: [], rows: AFTER.rows, newCols: newCols }
    ];
  }

  /* ---------- State + render ---------- */
  var currentStep = 0;
  var playing = false;
  var playTimer = null;

  function renderStep(idx) {
    var step = STEPS[idx];
    gridEl.innerHTML = "";

    var headerRow = document.createElement("div");
    headerRow.className = "grid-row";
    step.activeCols.forEach(function(col) {
      var cell = document.createElement("div");
      var cls = "cell header";
      if ((step.fadedCols || []).indexOf(col) >= 0) cls += " faded";
      if ((step.newCols || []).indexOf(col) >= 0) cls += " flag-new-header";
      cell.className = cls;
      var bg = COLORS[col] || "#999";
      cell.style.background = bg;
      cell.style.borderColor = bg;
      cell.textContent = col;
      headerRow.appendChild(cell);
    });
    gridEl.appendChild(headerRow);

    step.rows.forEach(function(row, rowIdx) {
      var tr = document.createElement("div");
      var rowCls = "grid-row";
      if ((step.droppedRows || []).indexOf(rowIdx) >= 0) rowCls += " row-faded";
      tr.className = rowCls;

      step.activeCols.forEach(function(col) {
        var cell = document.createElement("div");
        var cls = "cell data";
        if ((step.fadedCols || []).indexOf(col) >= 0) cls += " faded";
        var flag = step.rowFlags ? step.rowFlags[rowIdx] : null;
        if (flag === "keep") cls += " flag-keep";
        if (flag === "drop") cls += " flag-drop";
        if ((step.newCols || []).indexOf(col) >= 0) cls += " flag-new";
        if (step.highlightSource === col) cls += " flag-keep";
        cell.className = cls;
        cell.textContent = (row[col] != null) ? row[col] : "";
        tr.appendChild(cell);
      });
      gridEl.appendChild(tr);
    });

    labelEl.textContent = step.label;

    var dots = progressEl.querySelectorAll(".dot");
    for (var i = 0; i < dots.length; i++) {
      dots[i].classList.toggle("active", i === idx);
    }
    btnPrev.disabled = idx === 0;
    btnNext.disabled = idx === STEPS.length - 1;
  }

  function goTo(n) {
    currentStep = Math.max(0, Math.min(STEPS.length - 1, n));
    renderStep(currentStep);
  }

  function stopPlay() {
    playing = false;
    clearInterval(playTimer);
    btnPlay.innerHTML = "&#9654; Play";
  }

  STEPS.forEach(function(_, i) {
    var dot = document.createElement("div");
    dot.className = "dot";
    dot.addEventListener("click", function() { stopPlay(); goTo(i); });
    progressEl.appendChild(dot);
  });

  btnPrev.addEventListener("click", function() { stopPlay(); goTo(currentStep - 1); });
  btnNext.addEventListener("click", function() { stopPlay(); goTo(currentStep + 1); });
  btnPlay.addEventListener("click", function() {
    if (playing) { stopPlay(); return; }
    playing = true;
    btnPlay.innerHTML = "&#10074;&#10074; Pause";
    if (currentStep === STEPS.length - 1) goTo(0);
    playTimer = setInterval(function() {
      if (currentStep < STEPS.length - 1) goTo(currentStep + 1);
      else stopPlay();
    }, DUR);
  });

  /* ---------- Static-frame mode (used by animate_save_gif) ---------- */
  if (STATIC_STEP >= 0) {
    controlsEl.style.display = "none";
    progressEl.style.display = "none";
    goTo(STATIC_STEP);
    return;
  }

  goTo(0);
})();
</script>
'

# Render a payload into the final HTML string.
# static_step = -1 means interactive; >= 0 renders that frame with controls hidden.
.html_template <- function(payload, config, static_step = -1L) {
  payload$config <- config[c("duration", "show_disclosure")]
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  # The payload is embedded inside a <script> block. jsonlite does not escape
  # "</", so a data value containing "</script>" would close the block early
  # and break the page. Escaping "</" as "<\/" is inert in JSON/JS strings and
  # makes the embed safe for arbitrary cell contents.
  payload_json <- gsub("</", "<\\/", payload_json, fixed = TRUE)

  html <- .TEMPLATE
  html <- gsub("__ID__",           .new_id(),                       html, fixed = TRUE)
  html <- gsub("__PAYLOAD_JSON__", as.character(payload_json),      html, fixed = TRUE)
  html <- gsub("__STATIC_STEP__",  as.character(as.integer(static_step)), html, fixed = TRUE)
  html <- gsub("__BOX__",          as.character(config$box_size),   html, fixed = TRUE)
  html <- gsub("__FONT_SIZE__",    as.character(config$font_size),  html, fixed = TRUE)
  html <- gsub("__FONT_FAMILY__",  config$font_family,              html, fixed = TRUE)
  html <- gsub("__FONT_WEIGHT__",  as.character(config$font_weight),    html, fixed = TRUE)
  html <- gsub("__TRANSITION__",   as.character(config$transition),     html, fixed = TRUE)
  html <- gsub("__CANVAS_BG__",    config$canvas_bg,                    html, fixed = TRUE)
  html <- gsub("__CANVAS_PAD__",   as.character(config$canvas_padding), html, fixed = TRUE)
  html <- gsub("__TITLE_BG__",     config$title_bg,                     html, fixed = TRUE)
  html <- gsub("__TITLE_COLOR__",  config$title_color,                  html, fixed = TRUE)
  html <- gsub("__CELL_BG__",      config$cell_bg,                      html, fixed = TRUE)
  html <- gsub("__CELL_COLOR__",   config$cell_color,                   html, fixed = TRUE)
  html <- gsub("__HEADER_COLOR__", config$header_color,                 html, fixed = TRUE)
  html <- gsub("__BORDER_COLOR__", config$border_color,                 html, fixed = TRUE)
  html <- gsub("__BORDER_WIDTH__", as.character(config$border_width),   html, fixed = TRUE)
  html <- gsub("__BORDER_RADIUS__", as.character(config$border_radius), html, fixed = TRUE)
  html <- gsub("__GAP__",          as.character(config$cell_gap),       html, fixed = TRUE)
  html <- gsub("__CELL_OPACITY__", as.character(config$cell_opacity),   html, fixed = TRUE)
  html <- gsub("__KEEP_COLOR__",   config$keep_color,                   html, fixed = TRUE)
  html <- gsub("__DROP_COLOR__",   config$drop_color,                   html, fixed = TRUE)
  html <- gsub("__NEW_COLOR__",    config$new_color,                    html, fixed = TRUE)
  html
}

# Wrap an HTML string in our class so print() and knitr both behave.
.as_animate_html <- function(html_string, payload, config) {
  out <- htmltools::HTML(html_string)
  attr(out, "animate_payload") <- payload
  attr(out, "animate_config")  <- config
  class(out) <- c("animate_html", class(out))
  out
}
