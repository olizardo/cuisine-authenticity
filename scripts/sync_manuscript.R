#!/usr/bin/env Rscript
#' Synchronize Google Drive Manuscript with Local Tables and Figures
#' Cuisine Authenticity Project
#'
#' This script downloads the live Google Doc manuscript, injects publication-grade
#' APA tables and figures in-place using OpenXML DOM manipulation, and uploads
#' the updated document back to Google Drive without altering author typography or formatting.

suppressPackageStartupMessages({
  library(googledrive)
  library(here)
})

# 1. Configuration: Google Doc ID from URL
doc_id <- "1qU0OoUbKx_jQ6t1BvkSJ2F2mdbqmJbhqfyRs3SNdrNY"
live_docx <- here("draft_live.docx")
updated_docx <- here("draft_updated.docx")

message("[1/4] Ensuring table summaries in cache/ are up to date...")
source(here("scripts", "generate_md_tables.R"))

message("[2/4] Downloading live manuscript from Google Drive...")
drive_auth(email = "omarlizardo@gmail.com")
drive_download(as_id(doc_id), path = live_docx, overwrite = TRUE)

message("[3/4] Performing in-place XML injection of tables and figures...")
exit_code <- system2("python3", args = c(here("scripts", "sync_manuscript.py"), live_docx, updated_docx))
if (exit_code != 0) {
  stop("Error during in-place XML injection.")
}

message("[4/4] Uploading updated manuscript back to Google Drive...")
drive_update(as_id(doc_id), media = updated_docx)

# Clean up local temporary files
if (file.exists(live_docx)) unlink(live_docx)
if (file.exists(updated_docx)) unlink(updated_docx)
if (file.exists(here("test_download.docx"))) unlink(here("test_download.docx"))
if (file.exists(here("test_updated.docx"))) unlink(here("test_updated.docx"))
if (file.exists(here("doc_text.txt"))) unlink(here("doc_text.txt"))

message("Synchronization complete! Google Doc updated successfully.")
