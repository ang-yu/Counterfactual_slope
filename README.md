R code for the empirical analysis in <a href="https://arxiv.org/abs/2401.07000" target="_blank">Yu and Zhao (2024)</a>. <br /><br />
Using raw NLSY1979 data, first run "Data_preparation", which cleans the data. <br /><br />
The code for analysis is arranged as follows.

<table>
  <thead>
    <tr>
      <th>Table/Figure</th>
      <th>Analysis</th>
      <th>File name</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Figure 1</td>
      <td>GE thesis</td>
      <td>GE_parametric</td>
    </tr>
    <tr>
      <td>Table 1</td>
      <td>ST thesis (D=high school)</td>
      <td>ST_HS_parametric</td>
    </tr>
    <tr>
      <td>Table 2</td>
      <td>ST thesis (D=college attendance)</td>
      <td>ST_attendance_parametric</td>
    </tr>
    <tr>
      <td></td>
      <td>ST thesis (D=college degree)</td>
      <td>ST_completion_parametric</td>
    </tr>
    <tr>
      <td>Table B1</td>
      <td>GE thesis</td>
      <td>GE_ML</td>
    </tr>
  </tbody>
</table>

<style>
  table {
    border-collapse: collapse;
  }

  th, td {
    padding: 8px;
  }

  /* Add border between first and second rows */
  tbody tr:nth-child(2) {
    border-top: 2px solid black;
  }

  /* Add border between second last and last row */
  tbody tr:nth-last-child(2) {
    border-bottom: 2px solid black;
  }
</style>










