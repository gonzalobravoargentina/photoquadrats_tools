# Photoquadrats 📸🌊

**Photoquadrats** is a collection of R scripts for managing, processing, and analyzing metadata from photoquadrats, designed to streamline workflows for marine research and annotation platforms.

---

## 📂 Project Structure

### **STAMP_metadata_inPHOTO.R**  
🚨 *Warning:* Running this script will permanently stamp GPS positions onto your photos. Ensure you have backups of the original files before use.

#### Features:
- Extracts metadata from photos using the `exifr` package.
- Integrates GPS data from `.gpx` files.
- Merges dive computer and Paralenz camera data.
- Embeds GPS data into photo metadata using `exiftool`.

---

### **Photoquadrats_metadata.R**  
Generates metadata files compatible with **CoralNet** and **BIIGLE** platforms, simplifying photo annotation workflows.

#### Features:
- **BIIGLE Metadata:**
  - Extracts EXIF data.
  - Formats metadata fields to meet BIIGLE requirements.
  - Adds columns for ground distance and image area.
- **CoralNet Metadata:**
  - Formats metadata fields for CoralNet's requirements.
  - Adds standard fields (e.g., water quality, framing gear).
  - Prepares `.csv` files ready for upload.

---

### **Underwater_track.R**  
Analyzes underwater tracking data and visualizes dive paths and associated photo locations.

Tag photos along a GPS track on a map, enhancing the visualization and analysis of underwater data. This feature helps you geotag your photos, providing valuable insights into the geographical context of your underwater photoquadrats. If you'd like to explore the interactive HTML map, please download the ['map_withphotos.html'](https://github.com/gonzalobravoargentina/photoquadrats/blob/master/map_withphotos.html) file and open it in a web browser.

![psub_footer](https://github.com/gonzalobravoargentina/photoquadrats/blob/master/map_withphotos.png)
