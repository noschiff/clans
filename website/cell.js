class Cell {

  constructor(x, y, type, nation) {
    this.x = x;
    this.y = y;
    this.type = type;
    this.nation = nation;
    this.selected = false;
    this.highlighted = false;
  }


  // TODO: Figure out how the OCaml Cell JSON handles Cell coords, and adjust accordingly.
  /**
   * Constructs and returns a cell object from the JSON string given.
   * @param {String} json 
   * @returns A cell constructed from JSON.
   */
  static fromJSON(json) {
    var parsed = JSON.parse(json);
    var type = parsed.type;
    var nation;
    if (parsed.hasOwnProperty('nation')) {
      nation = parsed.nation;
    } else {
      nation = null;
    }

    return new Cell(0, 0, type, nation);
  }

  /**
   * Gets the Hex-Code Color for this cell based on its properties.
   * @returns The hex-code color.
   */
  getColor() {
    if (this.selected) {
      return "#00FFFF";
    }
    if (this.highlighted) {
      return "#4dcbff";
    }
    switch (this.type) {
      case "empty":
        return "#e0f6ff";
      case "wall":
        return "#000000";
      case "life":
        return "#00FF00";
    }
  }
}