const CELL_SIZE = 5;
var WIDTH = 100;
var HEIGHT = 100;

var world = undefined;
var highlightedCell = undefined;
var selectedCell = undefined;

var canvas = document.getElementById("sheet");
var ctx = canvas.getContext("2d");
let simulationButton = document.getElementById("btnRunSimulation");
let simulationSpeed = document.getElementById("txtFps");
let simulationTimer = undefined;

/**
 * Sets the highlighted cell, creating a + shape at its coordinates.
 * @param {CanvasRenderingContext2D} canvasContext 
 * @param {Cell} cell 
 */
function setHighlightedCell(canvasContext, cell) {
  if (highlightedCell != undefined) {
    highlightedCell.highlighted = false;

    toggleCellHighlight(canvasContext, highlightedCell.x, highlightedCell.y, false);
    toggleCellHighlight(canvasContext, highlightedCell.x + 1, highlightedCell.y, false);
    toggleCellHighlight(canvasContext, highlightedCell.x - 1, highlightedCell.y, false);
    toggleCellHighlight(canvasContext, highlightedCell.x, highlightedCell.y + 1, false);
    toggleCellHighlight(canvasContext, highlightedCell.x, highlightedCell.y - 1, false);
  }
  highlightedCell = cell;
  if (cell != undefined) {

    toggleCellHighlight(canvasContext, cell.x, cell.y, true);
    toggleCellHighlight(canvasContext, cell.x + 1, cell.y, true);
    toggleCellHighlight(canvasContext, cell.x - 1, cell.y, true);
    toggleCellHighlight(canvasContext, cell.x, cell.y + 1, true);
    toggleCellHighlight(canvasContext, cell.x, cell.y - 1, true);
  }
}

/**
 * Toggles a cell highlight at the specified cell grid coordinates
 * @param {CanvasRenderingContext2D} canvasContext 
 * @param {Number} x 
 * @param {Number} y 
 * @param {Boolean} toggled 
 */
function toggleCellHighlight(canvasContext, x, y, toggled) {
  if (world.isInBounds(x, y)) {
    var cell = world.getCell(x, y);
    cell.highlighted = toggled;
    renderCellAtCoordinate(canvasContext, cell.x, cell.y);

  }
}

/**
 * Sets the selected cell object, highlighting it and displaying its information.
 * @param {CanvasRenderingContext2D} canvasContext 
 * @param {Cell} cell 
 */
function setSelectedCell(canvasContext, cell) {
  if (selectedCell != undefined) {
    selectedCell.selected = false;
    renderCellAtCoordinate(canvasContext, selectedCell.x, selectedCell.y);
  }
  selectedCell = cell;
  if (cell != undefined) {
    cell.selected = true;

    updateCellInformation();
    renderCellAtCoordinate(canvasContext, cell.x, cell.y);
    if (cell.type == "life") {
      document.getElementById("btnEditCell").innerText = "Edit Cell";
      document.getElementById("lblEditCell").innerText = "Edit Cell @ (" + cell.x + "," + cell.y + ")"
      document.getElementById("btnEditCell").disabled = false;
    } else {
      document.getElementById("btnEditCell").disabled = true;
    }
  }
}

/**
 * Updates the Cell Information right sidebar with the selectedCell information.
 */
function updateCellInformation() {
  if (selectedCell != undefined) {
    document.getElementById('cell_coordinate').innerHTML = "Coordinate: (" + selectedCell.x + "," + selectedCell.y + ")";
    document.getElementById('cell_type').innerHTML = "Type: " + selectedCell.type;
    document.getElementById('cell_energy').innerHTML = `Energy: ${selectedCell.energy}`;
    if (selectedCell.type == "life") {
      document.getElementById('cell_nation').innerHTML = "Nation: " + selectedCell.nation;
    } else {
      document.getElementById('cell_nation').innerHTML = "Nation: N/A";
    }
  }
}

/**
 * Gets the cell at the specified canvas coords.
 * These coordinates are NOT cell coordinates.
 * @param {Number} x 
 * @param {Number} y 
 * @returns The Cell object at the canvas coords
 */
function getCellFromCanvasCoords(x, y) {
  if (!world.isInBounds(Math.floor(x / CELL_SIZE), Math.floor(y / CELL_SIZE))) {
    return null;
  }
  return world.getCell(Math.floor(x / CELL_SIZE), Math.floor(y / CELL_SIZE));
}

/**
 * Renders the cell at the specified cell coordinate onto the canvasContext
 * @param {CanvasRenderingContext2D} canvasContext 
 * @param {Number} x 
 * @param {Number} y 
 */
function renderCellAtCoordinate(canvasContext, x, y) {
  if (!world.isInBounds(x, y)) {
    return;
  }

  var cell = world.getCell(x, y);
  canvasContext.fillStyle = cell.getColor();
  canvasContext.fillRect(x, y, 1, 1);

}

/**
 * Renders the entire Canvas onto the specified canvasContext.
 * The rendering works by pre-rendering the canvas onto an off-screen canvas,
 * then drawing it onto the main canvas.
 * WARNING: This is a performance-intensive method.
 * @param {CanvasRenderingContext2D} canvasContext 
 */
function renderCanvas(canvasContext) {

  var m_canvas = document.createElement('canvas');
  m_canvas.width = WIDTH * CELL_SIZE;
  m_canvas.height = HEIGHT * CELL_SIZE;
  var m_context = m_canvas.getContext('2d');

  for (let i = 0; i < HEIGHT; i++) {
    for (let k = 0; k < WIDTH; k++) {

      var cell = world.getCell(k, i);
      m_context.fillStyle = cell.getColor();
      m_context.fillRect(k, i, 1, 1);
    }
  }

  canvasContext.drawImage(m_canvas, 0, 0);
}

/**
 * Auto-toggles the cell nation input field based on the cell type dropdown.
 * If the dropdown value is "life", which represents a living cell, then it
 * will toggle it on. 
 * Else, it will disable it.
 */
function toggleCellNationInput() {
  var cellTypeDropdown = document.getElementById("selCellType");
  var txtCellNation = document.getElementById("txtCellNation");

  if (cellTypeDropdown.value == "life") {
    txtCellNation.disabled = false;
    txtCellNation.value = selectedCell.nation;
  } else {
    txtCellNation.disabled = true;
    txtCellNation.value = "";
  }
}

/**
 * The event handler code for whenever the user clicks on the "Open World File"
 * button.
 */
function selectWorldFile() {
  document.getElementById('inpOpenWorldFile').click();
}

/**
 * Prompts an open file screen to open a file to read its contents from.
 * This should be used for opening ONlY JSON files.
 */
function openWorldFile() {
  var input = document.getElementById('inpOpenWorldFile');

  if (input.files.length > 0) {
    var file = input.files[0];
    const reader = new FileReader();
    reader.onload = event => {
      fetch('http://localhost:3000/load', {
        method: 'POST',
        body: JSON.stringify({
          world: event.target.result
        })
      }).then(response => {
        console.log(response);
        if (response.ok) {
          return response.json();
        }
        throw new Error("Not OK");
      }).then(data => {
        renderWorld(data);

      })
        .catch(reason => {
          console.log("Could not get info from http://localhost:3000/load. Error: " + reason)
        });
    }
    reader.onerror = error => reject(error)
    reader.readAsText(file);
  } else {
    console.log("No file...");
  }
}

function renderWorld(data) {
  console.log(data);

  for (let x = 0; x < data.world.cells.length; x++) {

    var cellRow = data.world.cells[x];
    for (let y = 0; y < cellRow.length; y++) {
      world.setCell(x, y, Object.assign(world.getCell(x, y), cellRow[y]));
    }
  }
  world.data = data;
  renderCanvas(ctx);
}

const jsonData = {
  name: "Jonth",
  email: "jobtd@mail.com",
  website: "www.4codev.com"
};

function downloadWorld() {
  const a = document.createElement("a");
  const file = new Blob([JSON.stringify(world.data)], { type: "text/plain" });
  a.href = URL.createObjectURL(file);
  a.download = "clans_world.json";
  a.click();
}

/**
 * Steps the simulation the amount of times specified by stepCount by sending a GET request
 * to the server, and retrieving the new/changed world state.
 * @param {Number} stepCount An integer step count. This can be 0, but not less than 0 
 * @param {Boolean} fullWorld True if the GET request should return the entire world state, false if otherwise. 
 * This is used for when needing to get the world upon loading the view.
 */
function stepSimulation(stepCount, fullWorld) {
  fetch('http://localhost:3000/step', {
    method: 'POST',
    body: JSON.stringify({
      full: fullWorld,
      steps: stepCount
    })
  }).then(response => {
    console.log(response);
    if (response.ok) {
      return response.json();
    }
    throw new Error("Not OK");
  }).then(data => {
    renderWorld(data);

  })
    .catch(reason => {
      console.log("Could not get info from http://localhost:3000/step. Error: " + reason)
    });
}

function runSimulation() {
  if (simulationTimer != undefined) {
    clearInterval(simulationTimer);
    simulationTimer = undefined;
    simulationSpeed.disabled = false;
    simulationButton.innerHTML = "Run Simulation";
  } else {
    stepSimulation(1, false);
    simulationTimer = setInterval(function () {
      stepSimulation(1, false);
    }, 1000 / simulationSpeed.value);
    simulationSpeed.disabled = true;
    simulationButton.innerHTML = "Stop Simulation";
  }
}

function postCellInformation(data) {
  console.log(JSON.stringify(data));
  fetch('http://localhost:3000/update_cell', {
    method: 'POST',
    body: JSON.stringify(data)
  }).then(response => {

    if (response.ok) {
      return response.json();
    }
    throw new Error("Not OK");
  })
    .then(data => {
      renderWorld(data);
    })
    .catch(reason => {
      console.log("Could not get info from http://localhost:3000/update_cell. Error: " + reason)
    });
}

function populate() {
  fetch('http://localhost:3000/populate', {
    method: 'POST',
    body: JSON.stringify({
      density: 0.9
    })
  }).then(response => {
    console.log(response);
    if (response.ok) {
      return response.json();
    }
    throw new Error("Not OK");
  })
    .then(data => {
      renderWorld(data);
    })
    .catch(reason => {
      console.log("Could not get info from http://localhost:3000/populate. Error: " + reason)
    });
}

/**
 * Essentially the main(String[] args) of the website.
 */
window.onload = function () {

  world = new World(WIDTH, HEIGHT);

  canvas.width = WIDTH * CELL_SIZE;
  canvas.height = HEIGHT * CELL_SIZE;
  ctx.imageSmoothingEnabled = false;
  ctx.scale(CELL_SIZE, CELL_SIZE);

  stepSimulation(0, true);

  canvas.addEventListener("mousemove", function (e) {
    if (!e) e = window.event;
    var x = e.offsetX == undefined ? e.layerX : e.offsetX;
    var y = e.offsetY == undefined ? e.layerY : e.offsetY;

    var cell = getCellFromCanvasCoords(x, y);
    setHighlightedCell(ctx, cell);
  });

  canvas.addEventListener("mousedown", function (e) {
    if (!e) e = window.event;
    var x = e.offsetX == undefined ? e.layerX : e.offsetX;
    var y = e.offsetY == undefined ? e.layerY : e.offsetY;
    var cell = getCellFromCanvasCoords(x, y);
    setSelectedCell(ctx, cell);
  });

  document.getElementById("btnEditCell").addEventListener("click", function () {
    var txtCellEnergy = document.getElementById("txtCellEnergy");
    txtCellEnergy.value = selectedCell.energy;
  });

  document.getElementById("btnSaveCell").addEventListener("click", function () {

    var txtCellEnergy = document.getElementById("txtCellEnergy");
    selectedCell.type = "life";
    selectedCell.energy = parseInt(txtCellEnergy.value);
    postCellInformation(Object.assign(selectedCell.data, {
      id: parseInt(selectedCell.x + "" + selectedCell.y),
      x: selectedCell.x,
      y: selectedCell.y,
      type: "life",
      energy: selectedCell.energy,
      nation: selectedCell.nation
    }));

    updateCellInformation();
    renderCellAtCoordinate(ctx, selectedCell.x, selectedCell.y);
  });
}