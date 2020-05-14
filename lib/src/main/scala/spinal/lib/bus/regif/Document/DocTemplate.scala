package spinal.lib.bus.regif

object DocTemplate {
  object cssThemes {
    val Default =
      """
        |      .theme-default {
        |          border: 3px solid #000;
        |          border-collapse: collapse;
        |      }
        |      .theme-default td,
        |      .theme-default th{
        |          border: 1px solid #000;
        |          border-top: 1px dashed #555;
        |          border-bottom: 1px dashed #555;
        |          padding: 3px;
        |      }
        |      .theme-default th{
        |          background: #bbb;
        |      }
        |      .theme-default tbody td.reserved{
        |          color: #bbb;
        |          font-weight:200;
        |          background : #eee;
        |          /* text-decoration:line-through; */
        |          text-decoration-color:#888;
        |      }
        |      .theme-default tbody tr.reg{
        |          border-top: 2px solid #000;
        |      }
        |""".stripMargin
    val Spring =
      """
        |      .theme-spring{
        |          border-collapse: collapse;
        |          font-size: 1.em;
        |          min-width: 800px;
        |          border-radius: 5px 5px 0 0 ;
        |          overflow: hidden;
        |          box-shadow: 0 -10px 20px rgba(0,0,0,0.15);
        |      }
        |      .theme-spring th,
        |      .theme-spring td {
        |          padding:5px 10px;
        |      }
        |      .theme-spring thead tr {
        |          background-color: #009879;
        |          color: #ffffff;
        |          text-align:center;
        |          font-weight: bold;
        |      }
        |      .theme-spring tbody tr{
        |          border-bottom: 1px solid #ddd;
        |      }
        |      .theme-spring tbody td{
        |          border: 1px solid #ddd;
        |      }
        |      .theme-spring tbody tr:last-of-type{
        |          border-bottom: 3px solid #009879;
        |      }
        |      .theme-spring tbody tr.active-row {
        |          font-weight: blod;
        |          color: #009879;
        |      }
        |      .theme-spring tbody td.reserved{
        |          color: #aaa;
        |          background : #fffff0;
        |          /* font-style:italic; */
        |          font-weight:200;
        |          font-size:1.0em;
        |      }
        |      .theme-spring tbody tr.green{
        |          background :#fffff0 ;
        |      }
        |""".stripMargin
  }

  val commonCSS =
    """
      |      body{ font-size: 0.8em; }
      |      p.regif-title{
      |          font-weight:800;
      |          font-size:1.2em;
      |      }
      |      td{
      |          white-space:pre-line; word-wrap: break-word; word-break: break-all;
      |      }
      |      td.fixWidth{
      |          min-width:50px;
      |          max-width:300px;
      |      }
      |      td.fixWidth2{
      |          min-width:50px;
      |          max-width:400px;
      |      }
      |      footer div p.info{
      |          font-weight:300;
      |          font-size:0.7em;
      |      }
      |      a {
      |        color:black;text-decoration:none;
      |      }
      |      a:hover {
      |          color:#09f;
      |      }
      |""".stripMargin
  val tableHead =
    """
      |      <thead>
      |        <tr align="center" >
      |          <th>AddressOffset</th>
      |          <th>RegName</th>
      |          <th>Description</th>
      |          <th>Width</th>
      |          <th>Section</th>
      |          <th>FieldName</th>
      |          <th>R/W</th>
      |          <th>Reset value</th>
      |          <th>Field-Description</th>
      |        </tr>
      |      <thead>
      |""".stripMargin

  def getHTML(moduleName: String, tbody: String): String = s"""
       |<!DOCTYPE html>
       |<html>
       |  <head>
       |    <title>
       |      ${moduleName}
       |    </title>
       |    <style>
       |      div{
       |          text-align: center;
       |      }
       |${commonCSS}
       |${cssThemes.Default}
       |${cssThemes.Spring}
       |    </style>
       |  </head>
       |  <body>
       |  <header align="center">
       |  <p class="regif-title"> ${moduleName} Interface Document </p>
       |  </header>
       |  <div class="table">
       |  <table  align="center" class="theme-default">
       |      <br/>
       |${tableHead}
       |      <tbody>
       |${tbody}
       |      </tbody>
       |  </table>
       |  </div>
       |  <footer align="center">
       |  <div> <p class="info">Powered By <a href="https://spinalhdl.github.io/SpinalDoc-RTD/"> SpinalHDL </a> </p> </div>
       |  </footer>
       |  </body>
       |</html>
       |""".stripMargin
}
