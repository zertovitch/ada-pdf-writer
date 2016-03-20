private package PDF_Out.Images is

  --  Gives the image index for a given file name.
  --  A new index will be created if the image is not yet registered.
  --
  procedure Image_ref(pdf: in out PDF_Out_Stream; file_name: String; image_index: out Positive);

  function Get_pixel_dimensions(image_file_name: String) return Rectangle;

  procedure Clear_local_resource_flags( pdf: PDF_Out_Stream );

  generic
    with procedure Action_private( dn: in out Dir_node );
    -- Dir_node is private: only known to us, contents subject to change
  procedure Traverse_private( pdf: PDF_Out_Stream );

  procedure Insert_unloaded_local_images( pdf: in out PDF_Out_Stream );

  procedure Clear_image_directory( pdf: in out PDF_Out_Stream );

end PDF_Out.Images;
