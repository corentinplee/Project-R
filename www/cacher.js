function myFunction(){
  glob = 0;
  if(glob === 0){
  document.getElementById("Nouvelledivision").style.display = "none";
  glob = 1;
  }
  if (glob > 0){
  document.getElementById("Nouvelledivision").style.display = "none";  
  }
  return glob;
}