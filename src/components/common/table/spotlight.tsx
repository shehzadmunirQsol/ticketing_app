import * as React from 'react';
import {
  ColumnDef,
  ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown, MoreHorizontal } from 'lucide-react';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { Button } from '@/ui/button';
import { Checkbox } from '@/ui/checkbox';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import Image from 'next/image';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
import { Switch } from '~/components/ui/switch';
import { SettingDialog } from '../modal/setting';
import { LoadingDialog } from '../modal/loadingModal';
import LanguageSelect, { LanguageInterface } from '../language_select';

export default function DataTableSpotLight() {
  const initialOrderFilters: any = {
    startDate: null,
    endDate: null,
    searchQuery: '',
    group: 'WONDER',
    lang_id: 1,

    rows: 10,
    first: 0,
    page: 0,
  };
  const [orderFilters, setOrderFilters] = React.useState({
    ...initialOrderFilters,
  });
  const { data, refetch, isLoading } = trpc.settings.get_banner.useQuery(
    orderFilters,
    {
      refetchOnWindowFocus: false,

      // enabled: user?.id ? true : false,
    },
  );
  const [sorting, setSorting] = React.useState<SortingState>([]);

  const [columnFilters, setColumnFilters] = React.useState<ColumnFiltersState>(
    [],
  );
  const [columnVisibility, setColumnVisibility] =
    React.useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = React.useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);
  const [type, setType] = React.useState('');

  const handleEnbled = (data: any, type: string) => {
    // console.log({ e, data });
    setSelectedItem(data);
    setTitle('Spot Light');
    setType(type);
    setIsModal(true);
  };
  const columns: ColumnDef<any>[] = [
    {
      id: 'title',
      header: 'Title',

      cell: ({ row }) => {
        const payment =
          row?.original?.value && JSON?.parse(row?.original?.value);

        return (
          <>
            <div className="flex items-center  gap-2">
              <Image
                className="object-contain bg-ac-2 h-10 w-16 rounded-lg"
                src={renderNFTImage(payment)}
                alt={row?.original?.name}
                width={32}
                height={32}
              />

              <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
                {row?.original?.name}
              </p>
            </div>
          </>
        );
      },
    },
    {
      id: 'description',
      header: 'Description',

      cell: ({ row }) => {
        const payment =
          row?.original?.value && JSON?.parse(row?.original?.value);

        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {payment?.description}
          </p>
        );
      },
    },

    {
      id: 'link',
      header: 'Link',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.link}
          </p>
        );
      },
    },
    {
      id: 'is_enabled',
      header: 'Enabled',

      cell: ({ row }) => {
        return (
          <>
            <Switch
              checked={row?.original?.is_enabled}
              onCheckedChange={() => handleEnbled(row?.original, 'enabled')}
            />
          </>
        );
      },
    },

    {
      id: 'actions',
      enableHiding: false,
      header:"Actions",
      cell: ({ row }) => {
        const payment = row?.original;
        console.log(row?.original, 'row?.original');

        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0 ">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              
              <Link href={`/admin/settings/spotlight/edit/${payment?.id}`}>
                <DropdownMenuItem>Edit Spot Light</DropdownMenuItem>
              </Link>
              <DropdownMenuItem
                onClick={() => handleEnbled(row?.original, 'delete')}
              >
                Delete Spotlight
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const wondersData = React.useMemo(() => {
    return Array.isArray(data) ? data : [];
  }, [data]);

  const table = useReactTable({
    data: wondersData,
    columns,
    onSortingChange: setSorting,
    onColumnFiltersChange: setColumnFilters,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnFilters,
      columnVisibility,
      rowSelection,
    },
  });
  function languageHandler(params: LanguageInterface) {
    setOrderFilters((prevFilters: any) => ({
      ...prevFilters,
      lang_id: params.id,
    }));
  }

  return (
    <div className="w-full  ">
      <div className="flex justify-between items-center py-4">
        <div></div>
        <div className="flex gap-2">
          <LanguageSelect languageHandler={languageHandler} />

          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="outline" className="ml-auto">
                Columns <ChevronDown className="ml-2 h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {table
                .getAllColumns()
                .filter((column) => column.getCanHide())
                .map((column) => {
                  return (
                    <DropdownMenuCheckboxItem
                      key={column.id}
                      className="capitalize"
                      checked={column.getIsVisible()}
                      onCheckedChange={(value) =>
                        column.toggleVisibility(!!value)
                      }
                    >
                      {column.id}
                    </DropdownMenuCheckboxItem>
                  );
                })}
            </DropdownMenuContent>
          </DropdownMenu>
        </div>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
              {table.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table.getRowModel().rows?.length ? (
                table.getRowModel().rows.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                    className=""
                    dir={orderFilters?.lang_id == 1 ? 'ltr' : 'rtl'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id} className=" p-6">
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => table.previousPage()}
            disabled={!table.getCanPreviousPage()}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => table.nextPage()}
            disabled={!table.getCanNextPage()}
          >
            Next
          </Button>
        </div>
      </div>
      <SettingDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
