# ── Cell 12: R Shiny Interactive Dashboard ────────────────────────────────────
#
# Launches an interactive EDI monitoring dashboard covering all 5 EDICT projects:
#   Tab 1 — Global EDI Overview        (Project 6 — AI/Galway)
#   Tab 2 — European Deep-Dive         (Project 10 — Regulatory/Utrecht)
#   Tab 3 — Drift Detection            (Project 6 — AI/Galway)
#   Tab 4 — AI Fairness Audit          (Project 9 — Statistical Frameworks)
#   Tab 5 — Core Outcome Reporting     (Project 4 — Core Outcome Set/Basel)
#
# Prerequisites: R must be installed (https://cran.r-project.org/)
# R packages installed automatically on first run (shiny, shinydashboard, plotly, dplyr, DT)
#
# ─────────────────────────────────────────────────────────────────────────────

import subprocess
import sys
import os
import shutil

# ── 1. Check R is installed ───────────────────────────────────────────────────
r_path = shutil.which("Rscript")

if r_path is None:
    print("✗ R not found on PATH.")
    print("  Please install R from https://cran.r-project.org/")
    print("  Then restart VS Code and re-run this cell.")
else:
    print(f"✓ R found: {r_path}")

    # ── 2. Set working directory ──────────────────────────────────────────────
    notebook_dir = os.path.dirname(os.path.abspath("recruitment_bias_monitor.ipynb"))
    os.chdir(notebook_dir)
    print(f"✓ Working directory: {notebook_dir}")

    # ── 3. Check app.R exists ─────────────────────────────────────────────────
    app_path = os.path.join(notebook_dir, "app.R")
    if not os.path.exists(app_path):
        print(f"✗ app.R not found at: {app_path}")
        print("  Make sure app.R is saved in the same folder as this notebook.")
    else:
        print(f"✓ app.R found: {app_path}")

        # ── 4. Install R dependencies (runs once, skips if already installed) ──
        print("\nChecking R package dependencies...")
        install_script = """
packages <- c("shiny", "shinydashboard", "plotly", "dplyr",
               "ggplot2", "DT", "scales", "tidyr", "shinycssloaders")
missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
    cat("Installing:", paste(missing, collapse=", "), "\\n")
    install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
    cat("✓ Installation complete\\n")
} else {
    cat("✓ All R packages already installed\\n")
}
"""
        result = subprocess.run(
            [r_path, "--vanilla", "-e", install_script],
            capture_output=True, text=True
        )
        print(result.stdout)
        if result.returncode != 0:
            print("⚠ Package install warnings:", result.stderr[:500])

        # ── 5. Launch Shiny app ───────────────────────────────────────────────
        print("\n" + "─" * 60)
        print("  Launching EDI Monitoring Dashboard...")
        print("  The app will open in your browser automatically.")
        print("  Press Ctrl+C in the terminal to stop the app.")
        print("─" * 60 + "\n")

        launch_cmd = f"""
setwd("{notebook_dir.replace(chr(92), '/')}")
shiny::runApp("app.R", launch.browser = TRUE, port = 8787)
"""
        subprocess.Popen(
            [r_path, "--vanilla", "-e", launch_cmd],
            creationflags=subprocess.CREATE_NEW_CONSOLE if sys.platform == "win32" else 0
        )

        print("✓ Dashboard launched — check your browser at http://127.0.0.1:8787")
        print("  (If browser does not open automatically, paste the URL above manually)")

print("\n✓ Cell 12 complete")
