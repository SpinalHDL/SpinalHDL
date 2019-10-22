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
        |      .theme-default td.fixwidth{
        |          min-width:50px;
        |          max-width:300px;
        |      }
        |      .theme-default td.fixwidth2{
        |          min-width:50px;
        |          max-width:400px;
        |      }
        |      .theme-default tbody td.reserved{
        |          color: #aaa;
        |          font-weight:200;
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
        |          /* background : #eee; */
        |          /* font-style:italic; */
        |          font-weight:200;
        |          font-size:1.0em;
        |      }
        |      .theme-spring tbody tr.green{
        |          background :#fffff0 ;
        |      }
        |""".stripMargin
  }

  val tableHead =
    """
      |      <thead>
      |        <tr align="center" >
      |          <th>addressOffset</th>
      |          <th>RegName</th>
      |          <th>Register Description</th>
      |          <th>width</th>
      |          <th>section</th>
      |          <th>FieldName</th>
      |          <th>R/W</th>
      |          <th>Reset value</th>
      |          <th>Field-description</th>
      |        </tr>
      |      <thead>
      |""".stripMargin

  def getHTML(moduleName: String, tbody: String): String = s"""
       |<!DOCTYPE html>
       |<html>
       |  <head>
       |    <title>
       |      Regif
       |    </title>
       |    <style>
       |      div{
       |          text-align: center;
       |      }
       |${cssThemes.Default}
       |${cssThemes.Spring}
       |    </style>
       |  </head>
       |  <body>
       |  <table  align="center" class="theme-default">
       |    <caption>${moduleName} Interface Document </caption>
       |      <br/>
       |${tableHead}
       |      <tbody>
       |${tbody}
       |      </tbody>
       |  </table>
       |  </body>
       |</html>
       |""".stripMargin
}
