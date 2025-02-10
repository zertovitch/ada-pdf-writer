--  K-means data clustering / unsupervised machine learning demo
--
--  https://en.wikipedia.org/wiki/K-means_clustering

with Ada.Numerics.Generic_Elementary_Functions;
with PDF_Out;

procedure K_Means is

  package Real_Math is
    new Ada.Numerics.Generic_Elementary_Functions (PDF_Out.Real);
  use Ada.Numerics, Real_Math, PDF_Out;

  doc : PDF_Out_File;

  page_center : Point;

  clusters : constant := 10;

  type Cluster_Range is range 1 .. clusters;

  multiple : constant := 13;

  data : array (1 .. multiple * clusters) of Point;

  alloc : array (data'Range) of Cluster_Range;

  procedure Prepare_Data is
    actual_center : array (Cluster_Range) of Point;
    radius_1 : constant := 200.0;
    radius_2 : constant :=  40.0;
    angle_1, angle_2 : Real;
    k : Integer;
    shift : constant := 4;
  begin
    page_center := (doc.Layout.width * 0.5, doc.Layout.height * 0.5);

    for i in Cluster_Range loop
      angle_1 := Real (i - 1) * 2.0 * Pi / Real (clusters);
      actual_center (i) :=
        page_center + radius_1 * (Cos (angle_1), Sin (angle_1));
      for j in 1 .. multiple loop
        angle_2 := Real (j) * 2.0 * Pi / Real (multiple);
        --  Create the data. The "correct" clustering is obvious:
        k := Integer (i - 1) * multiple + j;
        data (k) :=
          actual_center (i) + radius_2 * (Cos (angle_2), Sin (angle_2));
        --  Allocate cluster, of course in a "wrong" way (shifted):
        k := 1 + (k + shift - 1) mod data'Length;
        alloc (k) := i;
      end loop;
    end loop;

  end Prepare_Data;

  centroid : array (Cluster_Range) of Point;
  count : array (Cluster_Range) of Natural;

  procedure Compute_Centroids is
    cl : Cluster_Range;
  begin
    centroid := (others => (0.0, 0.0));
    count := (others => 0);
    --  Calculate centroid.
    for i in data'Range loop
      cl := alloc (i);
      centroid (cl) := centroid (cl) + data (i);
      count (cl) := count (cl) + 1;
    end loop;
    for c in Cluster_Range loop
      if count (c) = 0 then
        null;  --  The cluster is not used -> centroid is undefined in this case.
      else
        centroid (c) := 1.0 / Real (count (c)) * centroid (c);
      end if;
    end loop;
  end Compute_Centroids;

  procedure Reallocate is
    cluster, cluster_new : Cluster_Range;
    current_dist : Real;
    --  Here we choose a metric, e.g. L1 or L2.
    function Distance (P1, P2 : Point) return Real renames L1_Distance;
  begin
    for i in data'Range loop
      cluster := alloc (i);
      cluster_new := cluster;
      current_dist := Distance (centroid (cluster), data (i));
      for c in Cluster_Range loop
        if c /= cluster
          and then count (c) > 0
          and then Distance (centroid (c), data (i)) < current_dist
        then
          cluster_new := c;
        end if;
      end loop;
      alloc (i) := cluster_new;
    end loop;
  end Reallocate;

  procedure Display (title : String) is
    display_radius : constant := 8.0;

    darkslategray : constant := 16#2f4f4f#;
    forestgreen   : constant := 16#228b22#;
    red           : constant := 16#ff0000#;
    yellow        : constant := 16#ffff00#;
    lime          : constant := 16#00ff00#;
    royalblue     : constant := 16#4169e1#;
    aqua          : constant := 16#00ffff#;
    sandybrown    : constant := 16#f4a460#;
    blue          : constant := 16#0000ff#;
    hotpink       : constant := 16#ff69b4#;

    palette : constant array (1 .. 10) of RGB_Code_Range :=
      (darkslategray,
       forestgreen,
       red,
       yellow,
       lime,
       royalblue,
       aqua,
       sandybrown,
       blue,
       hotpink);

    procedure Paint_Square (c : Cluster_Range; half_side : Real) is
    begin
      doc.Draw
        ((centroid (c).x - half_side,
          centroid (c).y - half_side,
          2.0 * half_side,
          2.0 * half_side),
         fill_then_stroke);
    end Paint_Square;

  begin
    doc.Stroking_Color ((0.25, 0.0, 0.0));
    doc.Put_Line (title);

    --  Show the data points as discs:
    for i in data'Range loop
      doc.Filling_Color (Convert (palette (Integer (alloc (i)))));
      doc.Circle (data (i), display_radius, fill_then_stroke);
    end loop;

    --  Show the centroids as squares:
    for c in Cluster_Range loop
      doc.Filling_Color ((1.0, 1.0, 1.0));
      Paint_Square (c, display_radius);
      doc.Filling_Color (Convert (palette (Integer (c))));
      Paint_Square (c, display_radius * 0.75);
    end loop;
  end Display;

begin
  Prepare_Data;

  doc.Create ("k_means.pdf");
  doc.Page_Setup (A4_portrait);

  Compute_Centroids;
  Display ("Initial condition");

  for iter in 1 .. 10 loop
    Reallocate;
    --
    doc.New_Page;
    Compute_Centroids;
    Display ("Iteration" & iter'Image);
  end loop;

  doc.Close;
end K_Means;
