package body PDF_Out.Images is

  procedure Image_ref(pdf: in out PDF_Out_Stream; file_name: String; image_index: out Positive) is

    procedure Insert(file_name: String; node: in out p_Dir_node) is
    begin
      if node = null then
        pdf.img_count:= pdf.img_count + 1;
        node:= new Dir_node'
          ( (name_len          => file_name'Length,
             left              => null,
             right             => null,
             file_name         => file_name,
             image_index       => pdf.img_count,
             pdf_object_index  => 0,  --  0 = not yet insterted into the PDF stream
             local_resource    => True)
          );
          image_index:= pdf.img_count;
      elsif file_name > node.file_name then
        Insert( file_name, node.right );
      elsif file_name < node.file_name then
        Insert( file_name, node.left );
      else
        --  Name found, image was already referenced (above in the document)
        image_index:= node.image_index;
        node.local_resource:= True;
      end if;
    end Insert;
  begin
    Insert(file_name, pdf.img_dir_tree);
  end Image_ref;

  generic
    with procedure Action_private( dn: in out Dir_node );
    -- Dir_node is private: only known to us, contents subject to change
  procedure Traverse_private( pdf: PDF_Out_Stream );

  procedure Traverse_private( pdf: PDF_Out_Stream ) is

    procedure Traverse( p: p_Dir_node ) is
    begin
      if p /= null then
        Traverse(p.left);
        Action_private(p.all);
        Traverse(p.right);
      end if;
    end Traverse;

  begin
    Traverse(pdf.img_dir_tree);
  end Traverse_private;

  procedure Clear_local_resource_flag( dn: in out Dir_node ) is
  begin
    dn.local_resource:= False;
  end;

  procedure Clear_local_resource_flags( pdf: PDF_Out_Stream ) is
    procedure Traverse_and_clear is new Traverse_private(Clear_local_resource_flag);
  begin
    Traverse_and_clear(pdf);
  end;

  procedure Insert_Image_as_XObject(pdf: in out PDF_Out_Stream; image_index: Positive) is
  begin
    null; -- !!
  end Insert_Image_as_XObject;

end PDF_Out.Images;
